use std::fmt::{Display, Formatter};
use std::io::Cursor;
use std::ops::Range;

use anyhow::{bail, Result};

use crate::{
    ArgValue, Argument, Expression, Function, Instruction, InstructionDescription, MixedArgValue,
    NameStore, Statement,
    parse, INSTRUCTION_DESCRIPTIONS,
    OPCODE_CK, OPCODE_SET, OPCODE_GOTO, OPCODE_NOP,
    OPCODE_CMP, OPCODE_CALC, OPCODE_CALC2, OPCODE_SAVE, OPCODE_COPY,
    OPCODE_FOR, OPCODE_FOR2, OPCODE_NEXT, OPCODE_IFEL_CK, OPCODE_ELSE_CK, OPCODE_ENDIF,
    OPCODE_WHILE, OPCODE_EWHILE, OPCODE_DO, OPCODE_EDWHILE, OPCODE_SWITCH, OPCODE_CASE,
    OPCODE_BREAK, OPCODE_ESWITCH, OPCODE_EVT_END
};

const EMPTY_BLOCK: (&str, MixedArgValue) = ("block_size", MixedArgValue::KnownType(ArgValue::U16(0)));

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Warning,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Warning => "Warning",
            Self::Error => "Error",
        })
    }
}

#[derive(Debug, Clone)]
struct JumpTarget<'a> {
    label: &'a str,
    offset: usize,
    ifel_ctr: u8,
    loop_ctr: u8,
}

impl JumpTarget<'_> {
    fn offset(&self, source: usize) -> i16 {
        (self.offset as isize - source as isize) as i16
    }
}

#[derive(Debug, Clone)]
struct CompilationState<'a> {
    name_store: &'a NameStore,
    instructions: Vec<Instruction>,
    unresolved_gotos: Vec<(usize, &'a str)>,
    labels: Vec<JumpTarget<'a>>,
    offset: usize,
    ifel_ctr: u8,
    loop_ctr: u8,
    log: Vec<(Level, String)>,
    function_indexes: Vec<usize>,
}

impl<'a> CompilationState<'a> {
    const fn new(name_store: &'a NameStore) -> Self {
        Self {
            name_store,
            instructions: Vec::new(),
            unresolved_gotos: Vec::new(),
            labels: Vec::new(),
            offset: 0,
            ifel_ctr: u8::MAX,
            loop_ctr: u8::MAX,
            log: Vec::new(),
            function_indexes: Vec::new(),
        }
    }
    
    fn succeeded(&self) -> bool {
        self.log.is_empty()
    }
    
    fn completed(&self) -> bool {
        !self.log.iter().any(|log| log.0 == Level::Error)
    }
    
    fn add(&mut self, instruction: Instruction) {
        self.offset += instruction.size();
        self.instructions.push(instruction);
    }
    
    fn warn(&mut self, msg: impl Into<String>) {
        self.log.push((Level::Warning, msg.into()));
    }
    
    fn error(&mut self, msg: impl Into<String>) {
        self.log.push((Level::Error, msg.into()));
    }
    
    fn resolve_expression(&mut self, expr: &Expression) -> MixedArgValue {
        match expr {
            Expression::Identifier(ident) => {
                match self.name_store.get_value(ident) {
                    Some(value) => value.into(),
                    None => {
                        self.error(format!("Unknown identifier: {}", ident));
                        MixedArgValue::UnknownType(-1)
                    }
                }
            }
            Expression::Int(i) => MixedArgValue::UnknownType(*i),
            Expression::Var(id) => ArgValue::VariableIndex(*id).into(),
            Expression::BitFlags(flags) => {
                let mut value = 0i32;
                for flag in flags {
                    value |= self.resolve_expression(flag).as_int();
                }
                MixedArgValue::UnknownType(value)
            }
            Expression::Annotated(expr, _) => self.resolve_expression(expr), // the annotation is a comment added by the decompiler, so we can safely ignore it
        }
    }
    
    fn empty_block() -> Argument {
        Argument::named(String::from("block_size"), Expression::Int(0))
    }
    
    fn make_inst(&mut self, opcode: u8, args: &[(&str, MixedArgValue)]) -> Result<()> {
        let inst = Instruction::make_from_opcode(
            opcode,
            self.offset,
            &[],
            args,
        )?;
        self.add(inst);
        Ok(())
    }
    
    fn transform_args<'b>(&mut self, args: &'b [Argument]) -> (Vec<MixedArgValue>, Vec<(&'b str, MixedArgValue)>) {
        let mut positional_arguments = Vec::new();
        let mut keyword_arguments = Vec::new();
        
        for arg in args {
            let value = self.resolve_expression(&arg.value());
            if let Some(name) = arg.name() {
                keyword_arguments.push((name, value));
            } else {
                positional_arguments.push(value);
            }
        }
        
        (positional_arguments, keyword_arguments)
    }
    
    fn make_inst_for_args(&mut self, opcode: u8, args: &[Argument]) -> Result<()> {
        let (positional_arguments, keyword_arguments) = self.transform_args(args);
        let inst = Instruction::make_from_opcode(
            opcode,
            self.offset,
            &positional_arguments,
            &keyword_arguments,
        )?;
        self.add(inst);
        Ok(())
    }

    fn make_name_inst_for_args(&mut self, name: &str, args: &[Argument]) -> Result<()> {
        let (positional_arguments, keyword_arguments) = self.transform_args(args);
        let inst = Instruction::make_from_name(
            name,
            self.offset,
            &positional_arguments,
            &keyword_arguments,
        )?;
        self.add(inst);
        Ok(())
    }
    
    fn inc_ifel(&mut self) {
        self.ifel_ctr = self.ifel_ctr.overflowing_add(1).0;
    }
    
    fn dec_ifel(&mut self) {
        self.ifel_ctr = self.ifel_ctr.overflowing_sub(1).0;
    }
    
    fn inc_loop(&mut self) {
        self.loop_ctr = self.loop_ctr.overflowing_add(1).0;
    }
    
    fn dec_loop(&mut self) {
        self.loop_ctr = self.loop_ctr.overflowing_sub(1).0;
    }

    fn invalid_block_instruction(opcode: u8) -> String {
        match Self::instruction_name(opcode) {
            Some(name) => format!("{name} is not a valid instruction to start a block"),
            None => format!("Unknown opcode: {:02X}", opcode),
        }
    }

    fn instruction_name(opcode: u8) -> Option<&'static str> {
        INSTRUCTION_DESCRIPTIONS.get(opcode as usize).map(InstructionDescription::name)
    }

    const fn block_end_opcode(opcode: u8) -> Option<u8> {
        Some(match opcode {
            OPCODE_IFEL_CK => OPCODE_ENDIF,
            OPCODE_WHILE => OPCODE_EWHILE,
            OPCODE_DO => OPCODE_EDWHILE,
            OPCODE_FOR | OPCODE_FOR2 => OPCODE_NEXT,
            OPCODE_CASE => OPCODE_BREAK,
            OPCODE_SWITCH => OPCODE_ESWITCH,
            _ => return None,
        })
    }

    fn add_block_end(&mut self, opcode: u8) -> Result<()> {
        // because edwhile requires conditions, we don't handle it automatically
        if opcode == OPCODE_DO {
            bail!("Can't automatically insert edwhile for do blocks because we don't know what conditions to use");
        }

        let Some(end_opcode) = Self::block_end_opcode(opcode) else {
            bail!(Self::invalid_block_instruction(opcode));
        };

        self.make_inst(end_opcode, &[])
    }

    fn add_endif(&mut self, if_index: usize) -> Result<()> {
        // increase the if block size to account for the new instruction
        let if_inst = &mut self.instructions[if_index];
        let block_size = if_inst.arg("block_size").unwrap().as_int() + 2;
        if_inst.set_arg("block_size", ArgValue::U16(block_size as u16))?;

        self.make_inst(OPCODE_ENDIF, &[])
    }
    
    fn process_block_statements(&mut self, body: &'a [Statement]) -> Result<()> {
        let mut ifel_index = None;
        let mut if_block_index_for_endif_check = None;
        for stmt in body {
            // kind of a hack, but the block size calculation for ifel_ck depends on whether there's
            // an else_ck, because if so, we need to jump OVER that instruction and directly into its
            // block if our condition fails. so whenever we see an ifel_ck block, if the very next
            // statement we see is an else_ck, we need to adjust the block size accordingly
            let mut is_ifel = false;
            if let Statement::Block(head, _) = stmt {
                if let Statement::Instruction(name, _) = head.as_ref() {
                    if name == "ifel_ck" {
                        ifel_index = Some(self.instructions.len());
                        is_ifel = true;
                    } else if name == "else_ck" {
                        if let Some(index) = ifel_index {
                            // there could have been an endif inserted before the ifel_ck we expected
                            let ifel_inst = if self.instructions[index].name() == "endif" {
                                &mut self.instructions[index + 1]
                            } else {
                                &mut self.instructions[index]
                            };
                            // it would be nicer to get the size to add from the else_ck instruction
                            // itself, but we haven't created it yet
                            let block_size = ifel_inst.arg("block_size").unwrap().as_int() + 4;
                            ifel_inst.set_arg("block_size", ArgValue::U16(block_size as u16))?;
                        }
                    }
                }
            }
            
            if !is_ifel {
                ifel_index = None;
            }
            
            self.process_statement(stmt, &mut if_block_index_for_endif_check)?;
        }

        // if this block ended with an if block with no endif, insert it
        if let Some(if_block_index) = if_block_index_for_endif_check {
            self.add_endif(if_block_index)?;
        }
        
        Ok(())
    }
    
    fn process_statement(&mut self, stmt: &'a Statement, if_block_index_for_endif_check: &mut Option<usize>) -> Result<()> {
        if if_block_index_for_endif_check.is_some() && !matches!(stmt, Statement::Block(_, _)) {
            // we had an if block that didn't end in an endif, and now we know that it's not followed
            // by an else block, so we need to insert the block end instruction
            self.add_endif(if_block_index_for_endif_check.take().unwrap())?;
        }

        match stmt {
            Statement::Instruction(name, args) => {
                // special handling for control flow instructions
                match name.as_str() {
                    "ifel_ck" => {
                        self.inc_ifel();
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        self.make_inst_for_args(OPCODE_IFEL_CK, &args)
                    }
                    "else_ck" => {
                        self.dec_ifel();
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        self.make_inst_for_args(OPCODE_ELSE_CK, &args)
                    }
                    "endif" => {
                        self.dec_ifel();
                        self.make_inst_for_args(OPCODE_ENDIF, &args)
                    }
                    "while" => {
                        self.inc_loop();
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        args.push(Argument::named(String::from("condition_size"), Expression::Int(0)));
                        self.make_inst_for_args(OPCODE_WHILE, &args)
                    }
                    "ewhile" => {
                        self.dec_loop();
                        self.make_inst_for_args(OPCODE_EWHILE, &args)
                    }
                    "next" => {
                        self.dec_loop();
                        self.make_inst_for_args(OPCODE_NEXT, &args)
                    }
                    "switch" => {
                        // the switch does in fact modify the loop counter
                        self.inc_loop();
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        self.make_inst_for_args(OPCODE_SWITCH, &args)
                    }
                    "case" => {
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        self.make_inst_for_args(OPCODE_CASE, &args)
                    }
                    "eswitch" => {
                        self.dec_loop();
                        self.make_inst_for_args(OPCODE_ESWITCH, &args)
                    }
                    "do" => {
                        self.inc_loop();
                        let mut args = args.clone();
                        args.push(Self::empty_block());
                        self.make_inst_for_args(OPCODE_DO, &args)
                    }
                    "edwhile" => {
                        self.dec_loop();
                        let mut args = args.clone();
                        args.push(Argument::named(String::from("condition_size"), Expression::Int(0)));
                        self.make_inst_for_args(OPCODE_EDWHILE, &args)
                    }
                    _ => self.make_name_inst_for_args(name, args),
                }
            }
            Statement::GoTo(label) => {
                let (ifel_ctr, loop_ctr, offset) = if let Some(target) = self.labels.iter().find(|target| target.label == label) {
                    (target.ifel_ctr, target.loop_ctr, target.offset(self.offset))
                } else {
                    self.unresolved_gotos.push((self.instructions.len(), label));
                    (u8::MAX, u8::MAX, 4)
                };

                self.make_inst(
                    OPCODE_GOTO,
                    &[
                        ("ifel_ctr", ArgValue::U8(ifel_ctr).into()),
                        ("loop_ctr", ArgValue::U8(loop_ctr).into()),
                        ("offset", ArgValue::I16(offset).into()),
                    ],
                )
            }
            Statement::FlagCheck { bank, bit, value } | Statement::AnnotatedFlagCheck { bank, bit, value, .. } => {
                self.make_inst(
                    OPCODE_CK,
                    &[
                        ("bank", ArgValue::U8(*bank).into()),
                        ("bit", ArgValue::U8(*bit).into()),
                        ("value", ArgValue::Bool(*value).into()),
                    ],
                )
            }
            Statement::FlagSet { bank, bit, value } | Statement::AnnotatedFlagSet { bank, bit, value, .. } => {
                let value = self.resolve_expression(value);
                self.make_inst(
                    OPCODE_SET,
                    &[
                        ("bank", ArgValue::U8(*bank).into()),
                        ("bit", ArgValue::U8(*bit).into()),
                        ("value", value),
                    ],
                )
            }
            Statement::VarOperation { id, op, source } => {
                let var = ArgValue::VariableIndex(*id);
                
                match (op.arg_value(), source) {
                    (None, Expression::Int(value)) => {
                        self.make_inst(
                            OPCODE_SAVE,
                            &[
                                ("lhs", var.into()),
                                ("rhs", ArgValue::I16(*value as i16).into()),
                            ],
                        )
                    }
                    (None, Expression::Var(id)) => {
                        self.make_inst(
                            OPCODE_COPY,
                            &[
                                ("lhs", var.into()),
                                ("rhs", ArgValue::VariableIndex(*id).into()),
                            ],
                        )
                    }
                    (Some(op @ ArgValue::ArithmeticOperator(_)), Expression::Int(value)) => {
                        self.make_inst(
                            OPCODE_CALC,
                            &[
                                ("lhs", var.into()),
                                ("rhs", ArgValue::I16(*value as i16).into()),
                                ("op", op.into()),
                            ],
                        )
                    }
                    (Some(op @ ArgValue::ArithmeticOperator(_)), Expression::Var(id)) => {
                        self.make_inst(
                            OPCODE_CALC2,
                            &[
                                ("op", op.into()),
                                ("lhs", var.into()),
                                ("rhs", ArgValue::VariableIndex(*id).into()),
                            ]
                        )
                    }
                    (Some(op @ ArgValue::ComparisonOperator(_)), Expression::Int(value)) => {
                        self.make_inst(
                            OPCODE_CMP,
                            &[
                                ("lhs", var.into()),
                                ("rhs", ArgValue::I16(*value as i16).into()),
                                ("op", op.into()),
                            ],
                        )
                    }
                    _ => {
                        self.error(format!("Invalid expression for variable operation: {:?}", source));
                        Ok(())
                    }
                }
            }
            Statement::ForLoop(expr) => {
                self.inc_loop();
                match expr {
                    Expression::Int(i) => {
                        self.make_inst(
                            OPCODE_FOR,
                            &[
                                EMPTY_BLOCK, // this will be filled in by the block handler
                                ("count", ArgValue::U16(*i as u16).into()),
                            ],
                        )
                    }
                    Expression::Var(id) => {
                        self.make_inst(
                            OPCODE_FOR2,
                            &[
                                EMPTY_BLOCK,
                                ("count", ArgValue::VariableIndex(*id).into()),
                            ],
                        )
                    }
                    _ => {
                        self.error(format!("Invalid expression for for loop: {:?}", expr));
                        Ok(())
                    }
                }
            }
            Statement::Block(head, body) => {
                // if there was an if block without an endif just before this block, check if we
                // need to add one here
                if if_block_index_for_endif_check.is_some() {
                    if head.instruction_name() == "else_ck" {
                        // if blocks don't need an endif if they're followed by an else block
                        *if_block_index_for_endif_check = None;
                    } else {
                        self.add_endif(if_block_index_for_endif_check.take().unwrap())?;
                    }
                }

                let head_index = self.instructions.len();
                self.process_statement(head, if_block_index_for_endif_check)?;

                let opcode = self.instructions[head_index].opcode();
                
                let body_offset = self.offset;
                self.process_block_statements(body)?;

                // automatically insert block end instruction if it's not already present
                if !matches!(opcode, OPCODE_DO | OPCODE_ELSE_CK) {
                    match Self::block_end_opcode(opcode) {
                        Some(end_opcode) => {
                            // the is_block check ensures that if the last thing in our block was a
                            // nested block of the same kind, we don't interpret its end instruction
                            // as our own end instruction
                            if self.instructions.last().unwrap().opcode() != end_opcode || body.last().map(Statement::is_block).unwrap_or(true) {
                                if opcode == OPCODE_IFEL_CK {
                                    // can't automatically insert yet because we need to see if an
                                    // else is coming up next, so just remember to check on the
                                    // next instruction
                                    *if_block_index_for_endif_check = Some(head_index);
                                } else {
                                    self.add_block_end(opcode)?;
                                }
                            }
                        }
                        None => self.error(Self::invalid_block_instruction(opcode)),
                    }
                }
                
                {
                    // even though we might need this mut ref again in a minute, we need to drop it
                    // here so we can loop through the body below
                    let head_inst = &mut self.instructions[head_index];
                    let block_size = self.offset - body_offset + if head_inst.opcode() == OPCODE_ELSE_CK {
                        // unlike other blocks, else_ck counts the block size from the beginning of the
                        // instruction instead of the end
                        head_inst.size()
                    } else {
                        0
                    };
                    head_inst.set_arg("block_size", ArgValue::U16(block_size as u16))?;
                }

                let body_index = head_index + 1;
                if body_index < self.instructions.len() {
                    // calculate condition group size
                    let mut condition_size = 0usize;
                    if opcode == OPCODE_WHILE {
                        for inst in &self.instructions[body_index..] {
                            if !inst.is_condition() {
                                break;
                            }
                            
                            condition_size += inst.size();
                        }
                        
                        let condition_size = if condition_size > u8::MAX as usize {
                            self.error(format!("while loop condition group size is too large: {}", condition_size));
                            u8::MAX
                        } else {
                            condition_size as u8
                        };
                        
                        self.instructions[head_index].set_arg("condition_size", ArgValue::U8(condition_size))?;
                    } else if opcode == OPCODE_DO {
                        let mut edwhile_index = body_index;
                        let mut found = false;
                        for inst in &self.instructions[body_index..] {
                            if inst.opcode() == OPCODE_EDWHILE {
                                found = true;
                                continue;
                            }
                            
                            if !found {
                                edwhile_index += 1;
                                continue;
                            }
                            
                            if !inst.is_condition() {
                                break;
                            }

                            condition_size += inst.size();
                        }

                        let condition_size = if condition_size > u8::MAX as usize {
                            self.error(format!("do loop condition group size is too large: {}", condition_size));
                            u8::MAX
                        } else {
                            condition_size as u8
                        };

                        self.instructions[edwhile_index].set_arg("condition_size", ArgValue::U8(condition_size))?;
                    }
                }

                Ok(())
            }
            Statement::Label(label, inner) => {
                let target = JumpTarget {
                    label: label.as_str(),
                    offset: self.offset,
                    ifel_ctr: self.ifel_ctr,
                    loop_ctr: self.loop_ctr,
                };
                
                let mut gotos_found = Vec::new();
                for (i, missing_label) in &self.unresolved_gotos {
                    if *missing_label == label {
                        let goto = &mut self.instructions[*i];
                        goto.set_args(
                            &[
                                ("ifel_ctr", ArgValue::U8(target.ifel_ctr)),
                                ("loop_ctr", ArgValue::U8(target.loop_ctr)),
                                ("offset", ArgValue::I16(target.offset(goto.offset()))),
                            ]
                        )?;
                        gotos_found.push(*i);
                    }
                }
                
                if !gotos_found.is_empty() {
                    self.unresolved_gotos.retain(|(i, _)| !gotos_found.contains(i));
                }
                
                self.labels.push(target);
                
                self.process_statement(inner, if_block_index_for_endif_check)
            }
            Statement::BlankLine => Ok(()), // these "statements" only come from the decompiler and can be safely ignored
        }
    }
    
    const fn has_unresolved_gotos(&self) -> bool {
        !self.unresolved_gotos.is_empty()
    }
    
    fn process_function(&mut self, function: &'a Function) -> Result<usize> {
        self.function_indexes.push(self.instructions.len());
        
        let start_offset = self.offset;
        self.process_block_statements(function.body())?;

        // if the function doesn't end in an evt_end instruction, insert one implicitly
        if self.instructions.last().map(Instruction::opcode).unwrap_or(OPCODE_NOP) != OPCODE_EVT_END {
            self.make_inst(OPCODE_EVT_END, &[])?;
        }
        
        Ok(self.offset - start_offset)
    }
    
    pub fn validate(&self) -> Result<()> {
        if self.has_unresolved_gotos() {
            bail!("Attempted to validate while there are still unresolved gotos. First missing label: {}", self.unresolved_gotos[0].1);
        }
        
        if !self.completed() {
            bail!("Errors occurred during compilation:\n{}", self.log.iter().map(|(level, msg)| {
                format!("{}: {}", level, msg)
            }).collect::<Vec<_>>().join("\n"));
        }
        
        Ok(())
    }
    
    fn compile_range(&self, range: Range<usize>) -> Result<Vec<u8>> {
        let instructions = &self.instructions[range];
        let buf_size = instructions.iter().map(Instruction::size).sum::<usize>();
        let mut buf = vec![0u8; buf_size];
        let mut cursor = Cursor::new(&mut buf);
        for instruction in instructions {
            instruction.write(&mut cursor)?;
        }
        
        Ok(buf)
    }
    
    fn compile_functions(&self) -> Result<Vec<Vec<u8>>> {
        self.validate()?;
        
        let mut functions = Vec::with_capacity(self.function_indexes.len());
        for pair in self.function_indexes.windows(2) {
            functions.push(self.compile_range(pair[0]..pair[1])?);
        }
        
        // last function
        if let Some(&start) = self.function_indexes.last() {
            functions.push(self.compile_range(start..self.instructions.len())?);
        }
        
        Ok(functions)
    }

    fn compile(&self) -> Result<Vec<u8>> {
        self.validate()?;

        self.compile_range(0..self.instructions.len())
    }
}

#[derive(Debug, Clone)]
pub struct Compiler {
    name_store: NameStore,
    init_script: Option<Function>,
    exec_script: Vec<Function>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            name_store: NameStore::new(),
            init_script: None,
            exec_script: Vec::new(),
        }
    }
    
    pub fn parse<T: AsRef<str>>(&mut self, script: T) -> Result<()> {
        let functions = parse(script)?;
        for function in functions {
            if function.is_init() {
                if self.init_script.is_some() {
                    bail!("Multiple init scripts are not allowed");
                }
                self.init_script = Some(function);
            } else {
                let index = self.exec_script.len();
                let name = function.name().unwrap();
                self.name_store.add(String::from(name), ArgValue::FunctionIndex(index as u8));
                
                self.exec_script.push(function);
            }
        }
        
        Ok(())
    }
    
    fn make_state(&'_ self) -> CompilationState<'_> {
        CompilationState::new(&self.name_store)
    }
    
    pub fn compile_init(&self) -> Result<Vec<u8>> {
        let Some(ref init_script) = self.init_script else {
            bail!("Tried to compile init script, but none was found");
        };
        
        let mut state = self.make_state();
        state.process_function(init_script)?;
        state.compile()
    }
    
    pub fn compile_exec(&self) -> Result<Vec<u8>> {
        if self.exec_script.is_empty() {
            bail!("Tried to compile exec script, but none was found");
        }
        
        let num_functions = self.exec_script.len();
        let mut sizes = Vec::with_capacity(num_functions);
        let mut state = self.make_state();
        for function in &self.exec_script {
            sizes.push(state.process_function(function)?);
        }
        
        let mut code_buf = state.compile()?;
        
        let header_size = num_functions * 2;
        let mut final_buf = Vec::with_capacity(code_buf.len() + header_size);
        let mut offset = header_size as u16;
        final_buf.extend_from_slice(&offset.to_le_bytes());
        for size in sizes.into_iter().take(num_functions - 1) {
            offset += size as u16;
            final_buf.extend_from_slice(&offset.to_le_bytes());
        }
        final_buf.append(&mut code_buf);
        
        Ok(final_buf)
    }
    
    pub fn compile_functions(&self) -> Result<(Option<Vec<u8>>, Option<Vec<Vec<u8>>>)> {
        let init = match self.init_script.is_some() {
            true => Some(self.compile_init()?),
            false => None,
        };

        let exec = match self.exec_script.is_empty() {
            true => None,
            false => Some({
                let mut state = self.make_state();
                for function in &self.exec_script {
                    state.process_function(function)?;
                }
                
                state.compile_functions()?
            }),
        };

        Ok((init, exec))
    }
    
    pub fn compile(&self) -> Result<(Option<Vec<u8>>, Option<Vec<u8>>)> {
        let init = match self.init_script.is_some() {
            true => Some(self.compile_init()?),
            false => None,
        };
        
        let exec = match self.exec_script.is_empty() {
            true => None,
            false => Some(self.compile_exec()?),
        };
        
        Ok((init, exec))
    }
}