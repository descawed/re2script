use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use anyhow::{anyhow, Error, Result};

use crate::constants::{search_constant, NameStore};
use crate::instruction::{
    ArgValue, ArithmeticOperator, ComparisonOperator, Instruction,
    OPCODE_SET, OPCODE_IFEL_CK, OPCODE_ELSE_CK, OPCODE_CASE, OPCODE_GOTO,
};

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i32),
    Identifier(String),
    BitFlags(Vec<Expression>),
    Var(u8),
    Annotated(Box<Expression>, String), // hack to allow me to inject comments when building the AST from a decompiled script
}

impl Expression {
    pub fn from_value_rich(value: ArgValue, names: &NameStore, comment_ids: bool) -> Self {
        if let ArgValue::VariableIndex(id) = value {
            return Self::Var(id);
        }

        if let Some(constant) = names.get_name(value) {
            return Self::Identifier(constant);
        }

        let int = Self::Int(value.as_int());
        
        if comment_ids {
            match value {
                ArgValue::CharacterId(id) => {
                    return Self::Annotated(Box::new(int), String::from(id.name()));
                }
                ArgValue::Item(item) => {
                    return Self::Annotated(Box::new(int), String::from(item.name()));
                }
                _ => (),
            }
        }
        
        int
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::BitFlags(flags) => {
                for (i, flag) in flags.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", flag)?;
                }

                Ok(())
            }
            Self::Var(id) => write!(f, "var[{}]", id),
            Self::Annotated(expr, comment) => write!(f, "{} /* {} */", expr, comment),
        }
    }
}

impl From<ArgValue> for Expression {
    fn from(value: ArgValue) -> Self {
        if let ArgValue::VariableIndex(id) = value {
            return Self::Var(id);
        }

        if let Some(constant) = search_constant(value) {
            return Self::Identifier(String::from(constant));
        }

        Self::Int(value.as_int())
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    name: Option<String>,
    value: Expression,
}

impl Argument {
    pub const fn named(name: String, value: Expression) -> Self {
        Self {
            name: Some(name),
            value,
        }
    }

    pub const fn expr(value: Expression) -> Self {
        Self {
            name: None,
            value,
        }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(ref name) = self.name {
            write!(f, "{}: ", name)?;
        }
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    LeftShift,
    RightShift,
    SignedRightShift,
    Equal,
    NotEqual,
    BitMask,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Value,
}

impl Operator {
    pub const fn is_unary(&self) -> bool {
        matches!(self, Self::BitNot | Self::Value)
    }
}

impl From<ArithmeticOperator> for Operator {
    fn from(value: ArithmeticOperator) -> Self {
        use ArithmeticOperator::*;

        match value {
            Add => Self::Add,
            Sub => Self::Sub,
            Mul => Self::Mul,
            Div => Self::Div,
            Mod => Self::Mod,
            BitOr => Self::BitOr,
            BitAnd => Self::BitAnd,
            BitXor => Self::BitXor,
            BitNot => Self::BitNot,
            LeftShift => Self::LeftShift,
            RightShift => Self::RightShift,
            SignedRightShift => Self::SignedRightShift,
        }
    }
}

impl From<ComparisonOperator> for Operator {
    fn from(value: ComparisonOperator) -> Self {
        use ComparisonOperator::*;

        match value {
            Equal => Self::Equal,
            NotEqual => Self::NotEqual,
            LessThan => Self::LessThan,
            LessThanOrEqual => Self::LessThanOrEqual,
            GreaterThan => Self::GreaterThan,
            GreaterThanOrEqual => Self::GreaterThanOrEqual,
            BitMask => Self::BitMask,
            BooleanVariable => Self::Value,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Assign => "=",
            Self::Add => "+=",
            Self::Sub => "-=",
            Self::Mul => "*=",
            Self::Div => "/=",
            Self::Mod => "%=",
            Self::BitAnd => "&=",
            Self::BitOr => "|=",
            Self::BitXor => "^=",
            Self::BitNot => "~",
            Self::LeftShift => "<<=",
            Self::RightShift => ">>=",
            Self::SignedRightShift => ">>>=",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::BitMask => "&",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::Value => "",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlagOperator {
    Assign,
    Equal,
    NotEqual,
}

impl TryFrom<Operator> for FlagOperator {
    type Error = Error;

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        Ok(match value {
            Operator::Assign => Self::Assign,
            Operator::Equal => Self::Equal,
            Operator::NotEqual => Self::Equal,
            _ => return Err(anyhow!("Operator {:?} is not valid for flags", value)),
        })
    }
}

fn write_block(f: &mut Formatter<'_>, head: impl Display, body: &[Statement]) -> std::fmt::Result {
    write!(f, "{}\n{{\n", head)?;

    for stmt in body {
        let s = stmt.to_string().replace('\n', "\n\t");
        write!(f, "\t{}\n", s)?;
    }

    write!(f, "}}")
}

#[derive(Debug, Clone)]
pub enum Statement {
    Instruction(String, Vec<Argument>),
    GoTo(String),
    FlagOperation { bank: u8, bit: u8, value: bool, op: FlagOperator },
    VarOperation { id: u8, op: Operator, source: Expression },
    ForLoop(Expression),
    Block(Box<Statement>, Vec<Statement>),
    Label(String, Box<Statement>),
    BlankLine, // ugly hack to give me some control over the formatting when building the AST from a decompiled script
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GoTo(label) => write!(f, "goto {}", label),
            Self::ForLoop(source) => write!(f, "for {}", source),
            Self::Label(label, stmt) => write!(f, "{}:\n{}", label, stmt),
            Self::FlagOperation { bank, bit, value, op } => {
                if (*op == FlagOperator::Equal && !value) || (*op == FlagOperator::NotEqual && *value) {
                    // checking for flag to be false
                    write!(f, "!")?;
                }
                write!(f, "flag[{}][{}]", bank, bit)?;
                if *op == FlagOperator::Assign {
                    write!(f, " = {}", value)?;
                }
                Ok(())
            }
            Self::VarOperation { id, op, source } => {
                match op {
                    Operator::BitNot => write!(f, "var[{}] = ~var[{}]", id, id),
                    Operator::Value => write!(f, "var[{}]", id),
                    _ => write!(f, "var[{}] {} {}", id, op, source),
                }
            }
            Self::Instruction(name, args) => {
                write!(f, "{}", name)?;
                if args.is_empty() {
                    return Ok(());
                }

                write!(f, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, ")")
            }
            Self::Block(head, body) => write_block(f, head, body),
            Self::BlankLine => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionName {
    Init,
    Exec(String),
}

impl Display for FunctionName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Init => write!(f, "init"),
            Self::Exec(name) => write!(f, "function {}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: FunctionName,
    body: Vec<Statement>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_block(f, &self.name, &self.body)
    }
}

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
            if let [Some(flag), Some(bit), Some(value)] = instruction.args(["flag", "bit", "value"]) {
                let op = if instruction.opcode() == OPCODE_SET {
                    FlagOperator::Assign
                } else {
                    FlagOperator::Equal
                };

                return Statement::FlagOperation {
                    bank: flag.as_int() as u8,
                    bit: bit.as_int() as u8,
                    value: value.as_int() != 0,
                    op,
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
            
            if !self.suppress_nops || !instruction.is_nop() {
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
            
            let stmt = if instruction.is_block_start() {
                self.handle_block(instruction, &mut iterator, &mut 0)
            } else {
                self.make_statement(instruction)
            };
            body.push(stmt);
        }

        Function {
            name,
            body,
        }
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