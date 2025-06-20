use std::collections::HashSet;
use std::iter::Peekable;

use anyhow::Result;

use crate::ast::*;
use crate::constants::{NameStore, flag_description};
use crate::instruction::{
    ArgValue, Instruction,
    OPCODE_SET, OPCODE_IFEL_CK, OPCODE_ELSE_CK, OPCODE_CASE, OPCODE_GOTO,
    OPCODE_ENDIF, OPCODE_EWHILE, OPCODE_NEXT, OPCODE_BREAK, OPCODE_ESWITCH, OPCODE_EVT_END,
};

#[derive(Debug, Clone)]
pub struct ScriptFormatter {
    comment_ids: bool,
    all_args: bool,
    arg_keyword_threshold: usize,
    suppress_nops: bool,
    name_store: NameStore,
    label_offsets: Vec<usize>,
}

impl ScriptFormatter {
    pub fn new(comment_ids: bool, all_args: bool, arg_keyword_threshold: usize, suppress_nops: bool) -> Self {
        Self {
            comment_ids,
            all_args,
            arg_keyword_threshold,
            suppress_nops,
            name_store: NameStore::new(),
            label_offsets: Vec::new(),
        }
    }
    
    fn get_label_index(&self, target: usize) -> Option<usize> {
        self.label_offsets.iter().position(|offset| *offset == target)
    }

    fn make_statement_inner(&self, instruction: &Instruction) -> Statement {
        // we don't check for blocks here because handle_block is responsible for that
        // we also don't check for gotos because those need to be handled later after the AST is
        // built so we can insert labels
        if instruction.is_var_op() {
            if let [Some(ArgValue::VariableIndex(id)), Some(rhs), op] = instruction.args(["lhs", "rhs", "op"]) {
                let op: Operator = match op {
                    Some(ArgValue::ArithmeticOperator(a)) => a.into(),
                    Some(ArgValue::ComparisonOperator(c)) => c.into(),
                    _ => Operator::Assign,
                };

                return Statement::VarOperation {
                    id,
                    op,
                    source: Expression::from_value_rich(rhs, &self.name_store, self.comment_ids),
                };
            }
        }

        if instruction.is_flag_op() {
            if let [Some(bank), Some(bit), Some(value)] = instruction.args(["bank", "bit", "value"]) {
                let bank = bank.as_int() as u8;
                let bit = bit.as_int() as u8;
                
                return match (instruction.opcode(), flag_description(bank, bit)) {
                    (OPCODE_SET, Some(description)) if self.comment_ids => Statement::AnnotatedFlagSet { bank, bit, value: value.into(), annotation: String::from(description) },
                    (OPCODE_SET, _) => Statement::FlagSet { bank, bit, value: value.into() },
                    (_, Some(description)) if self.comment_ids => Statement::AnnotatedFlagCheck { bank, bit, value: value.as_int() != 0, annotation: String::from(description) },
                    _ => Statement::FlagCheck { bank, bit, value: value.as_int() != 0 },
                };
            }
        }

        if instruction.is_for_loop() {
            if let Some(count) = instruction.arg("count") {
                return Statement::ForLoop(count.into());
            }
        }
        
        if let Some(i) = Self::goto_offset(instruction).and_then(|o| self.get_label_index(o)) {
            return Statement::GoTo(format!("label{i}"));
        }

        let mut args = Vec::new();
        for (info, &value) in instruction.describe_args() {
            if info.should_show_value(value) || self.all_args {
                args.push((info, value));
            }
        }

        let show_keywords = args.len() > self.arg_keyword_threshold;

        Statement::Instruction(
            String::from(instruction.name()),
            args.into_iter().map(|(info, value)| {
                let value_expr = Expression::from_value_rich(value, &self.name_store, self.comment_ids);
                if show_keywords || info.is_keyword_only() {
                    Argument::named(String::from(info.name()), value_expr)
                } else {
                    Argument::expr(value_expr)
                }
            }).collect(),
        )
    }
    
    fn make_statement(&self, instruction: &Instruction) -> Statement {
        let stmt = self.make_statement_inner(instruction);
        if let Some(i) = self.get_label_index(instruction.offset()) {
            Statement::Label(format!("label{i}"), Box::new(stmt))
        } else {
            stmt
        }
    }

    fn is_sibling(head: &Instruction, child: Option<&&Instruction>, bytes_read: usize, block_size: usize) -> bool {
        let Some(child) = child else {
            return false;
        };
        let head_opcode = head.opcode();
        let child_opcode = child.opcode();
        matches!(
            (head_opcode, child_opcode),
            (OPCODE_IFEL_CK, OPCODE_ELSE_CK)
            | (OPCODE_CASE, OPCODE_CASE)
        ) && bytes_read + child.size() >= block_size
    }

    fn handle_block<'a>(&self, head: &'a Instruction, iterator: &mut Peekable<impl Iterator<Item=&'a Instruction>>, parent_bytes_read: &mut usize) -> Statement {
        let head_statement = self.make_statement(head);

        let mut block = Vec::new();
        let Some(block_size) = head.arg("block_size") else {
            return head_statement;
        };
        let block_size = block_size.as_int() as usize - if head.opcode() == OPCODE_ELSE_CK {
            // unlike other blocks, else_ck counts the block size from the beginning of the
            // instruction instead of the end
            head.size()
        } else {
            0
        };
        let mut bytes_read = 0usize;

        // check if this is a block instruction that should logically be a sibling of our
        // block instead of a child. if it is, break out of our block early and let that
        // instruction be the start of a new block
        let peek = iterator.peek();
        if Self::is_sibling(head, peek, bytes_read, block_size) {
            return head_statement;
        }
        
        // if we're inside a conditional block, insert a blank line once we reach the end of the
        // conditions to make it clearer where they begin and end
        let mut format_after_conditions = if let Some(&instruction) = peek {
            head.has_conditions() && instruction.is_condition()
        } else {
            false
        };

        while let Some(instruction) = iterator.next() {
            if format_after_conditions && !instruction.is_condition() {
                format_after_conditions = false;
                block.push(Statement::BlankLine);
            }
            
            bytes_read += instruction.size();
            let stmt = if instruction.is_block_start() {
                self.handle_block(instruction, iterator, &mut bytes_read)
            } else {
                if instruction.has_conditions() {
                    block.push(Statement::BlankLine);
                }
                self.make_statement(instruction)
            };

            // don't include nops if we're suppressing nops. likewise, unless we're displaying
            // everything (all_args), don't show block end instructions, as they're implied.
            // however, if there's a block end instruction that's NOT at the end of the block, we
            // still show that, as it's not implicit.
            let should_include = (!self.suppress_nops || !instruction.is_nop())
                &&
                (
                self.all_args || bytes_read < block_size ||
                    !matches!(instruction.opcode(), OPCODE_ENDIF | OPCODE_EWHILE | OPCODE_NEXT | OPCODE_BREAK | OPCODE_ESWITCH)
                );
            
            if should_include {
                block.push(stmt);
            }

            if bytes_read >= block_size || Self::is_sibling(head, iterator.peek(), bytes_read, block_size) {
                break;
            }
        }

        *parent_bytes_read += bytes_read;

        if block.is_empty() {
            return head_statement;
        }

        Statement::Block(Box::new(head_statement), block)
    }

    fn format_instructions(&self, name: FunctionName, instructions: &[Instruction]) -> Function {
        // this will almost certainly overestimate because instructions inside blocks will be pushed
        // down into the blocks, but it guarantees we won't need any extra allocations
        let mut body = Vec::with_capacity(instructions.len());
        let mut iterator = instructions.iter().peekable();

        while let Some(instruction) = iterator.next() {
            if self.suppress_nops && instruction.is_nop() {
                continue;
            }
            
            // unless we're showing everything (all_args), don't show the function end instruction,
            // as it's implicit
            if !self.all_args && instruction.opcode() == OPCODE_EVT_END {
                break;
            }
            
            let stmt = if instruction.is_block_start() {
                self.handle_block(instruction, &mut iterator, &mut 0)
            } else {
                self.make_statement(instruction)
            };
            body.push(stmt);
            
            // evt_end at the top level of a function is the end of the function
            if instruction.opcode() == OPCODE_EVT_END {
                break;
            }
        }

        Function::new(name, body)
    }

    fn format_script(&mut self, functions: &[Vec<Instruction>]) -> Vec<Function> {
        // add our generated function names to the name store
        for i in 0..functions.len() {
            self.name_store.add(format!("sub{i}"), ArgValue::FunctionIndex(i as u8));
        }
        
        let mut out = Vec::with_capacity(functions.len());
        for (i, function) in functions.iter().enumerate() {
            out.push(self.format_instructions(FunctionName::Exec(format!("sub{i}")), &function));
        }

        out
    }
    
    fn goto_offset(instruction: &Instruction) -> Option<usize> {
        match (instruction.opcode() == OPCODE_GOTO, instruction.arg("offset")?) {
            (true, ArgValue::I16(offset)) => instruction.offset().checked_add_signed(offset as isize),
            _ => None,
        }
    }
    
    fn collect_offsets(instructions: &[Instruction], offsets: &mut HashSet<usize>) {
        for instruction in instructions {
            offsets.insert(instruction.offset());
        }
    }
    
    fn find_labels(&mut self, instructions: &[Instruction], valid_offsets: &HashSet<usize>) {
        for instruction in instructions {
            let Some(target) = Self::goto_offset(instruction) else {
                continue;
            };
            
            if valid_offsets.contains(&target) && !self.label_offsets.contains(&target) {
                self.label_offsets.push(target);
            }
        }
    }
    
    pub fn reset(&mut self) {
        self.name_store.clear();
    }

    pub fn parse_init_script(&mut self, buf: &[u8]) -> Function {
        self.reset();
        
        let instructions = Instruction::read_function(buf);
        
        let mut valid_offsets = HashSet::new();
        Self::collect_offsets(&instructions, &mut valid_offsets);
        self.find_labels(&instructions, &valid_offsets);
        
        self.format_instructions(FunctionName::Init, &instructions)
    }

    pub fn parse_exec_script(&mut self, buf: &[u8]) -> Result<Vec<Function>> {
        self.reset();
        
        let script = Instruction::read_script(buf)?;

        let mut valid_offsets = HashSet::new();
        for function in &script {
            Self::collect_offsets(function, &mut valid_offsets);
        }
        for function in &script {
            self.find_labels(function, &valid_offsets);
        }
        
        Ok(self.format_script(&script))
    }
}