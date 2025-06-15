use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use anyhow::{anyhow, Error};

use crate::constants::search_constant;
use crate::instruction::{
    ArgValue, ArithmeticOperator, ComparisonOperator, Instruction,
    OPCODE_SET, OPCODE_IFEL_CK, OPCODE_ELSE_CK, OPCODE_CASE,
};

const NAMED_ARG_THRESHOLD: usize = 2;

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i32),
    Identifier(String),
    BitFlags(Vec<Expression>),
    Var(u8),
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    body: Vec<Statement>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_block(f, format!("function {}", self.name), &self.body)
    }
}

impl Function {
    fn make_statement(instruction: &Instruction) -> Statement {
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
                    source: rhs.into(),
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

        let mut args = Vec::new();
        for (info, &value) in instruction.describe_args() {
            if info.should_show_value(value) {
                args.push((info, value));
            }
        }

        let show_keywords = args.len() > NAMED_ARG_THRESHOLD;

        Statement::Instruction(
            String::from(instruction.name()),
            args.into_iter().map(|(info, value)| {
                if show_keywords || info.is_keyword_only() {
                    Argument::named(String::from(info.name()), value.into())
                } else {
                    Argument::expr(value.into())
                }
            }).collect(),
        )
    }
    
    fn is_sibling(head: &Instruction, child: Option<&&Instruction>) -> bool {
        let Some(child) = child else {
            return false;
        };
        let head_opcode = head.opcode();
        let child_opcode = child.opcode();
        matches!(
            (head_opcode, child_opcode),
            (OPCODE_IFEL_CK, OPCODE_ELSE_CK)
            | (OPCODE_CASE, OPCODE_CASE)
        )
    }

    fn handle_block<'a>(head: &'a Instruction, iterator: &mut Peekable<impl Iterator<Item=&'a Instruction>>, parent_bytes_read: &mut usize) -> Statement {
        let head_statement = Self::make_statement(head);

        let mut block = Vec::new();
        let Some(block_size) = head.arg("block_size") else {
            return head_statement;
        };
        let block_size = block_size.as_int() as usize;
        let mut bytes_read = 0usize;

        // check if this is a block instruction that should logically be a sibling of our
        // block instead of a child. if it is, break out of our block early and let that
        // instruction be the start of a new block
        if Self::is_sibling(head, iterator.peek()) {
            return head_statement;
        }

        while let Some(instruction) = iterator.next() {
            let stmt = if instruction.is_block_start() {
                Self::handle_block(instruction, iterator, &mut bytes_read)
            } else {
                bytes_read += instruction.size();
                Self::make_statement(instruction)
            };
            block.push(stmt);

            if bytes_read >= block_size || Self::is_sibling(head, iterator.peek()) {
                break;
            }
        }

        *parent_bytes_read += bytes_read;
        
        if block.is_empty() {
            return head_statement;
        }

        Statement::Block(Box::new(head_statement), block)
    }

    pub fn from_instructions<T: Into<String>>(name: T, instructions: &[Instruction]) -> Self {
        let name = name.into();

        // this will almost certainly overestimate because instructions inside blocks will be pushed
        // down into the blocks, but it guarantees we won't need any extra allocations
        let mut body = Vec::with_capacity(instructions.len());
        let mut iterator = instructions.iter().peekable();

        while let Some(instruction) = iterator.next() {
            let stmt = if instruction.is_block_start() {
                Self::handle_block(instruction, &mut iterator, &mut 0)
            } else {
                Self::make_statement(instruction)
            };
            body.push(stmt);
        }

        Self {
            name,
            body,
        }
    }

    pub fn from_script(functions: &[Vec<Instruction>]) -> Vec<Self> {
        let mut out = Vec::with_capacity(functions.len());
        for (i, function) in functions.iter().enumerate() {
            out.push(Self::from_instructions(format!("sub{i}"), &function));
        }

        out
    }
}