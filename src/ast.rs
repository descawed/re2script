use std::fmt::{Display, Formatter};
use crate::ArithmeticOperator::{Add, BitAnd, BitNot, BitOr, BitXor, Div, LeftShift, Mod, Mul, RightShift, SignedRightShift, Sub};
use crate::ComparisonOperator::{BitMask, BooleanVariable, Equal, GreaterThan, GreaterThanOrEqual, LessThan, LessThanOrEqual, NotEqual};
use crate::constants::{search_constant, NameStore};
use crate::instruction::{ArgValue, ArithmeticOperator, ComparisonOperator};

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
    pub const fn new(name: Option<String>, value: Expression) -> Self {
        Self {
            name,
            value,
        }
    }
    
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
    
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }
    
    pub const fn value(&self) -> &Expression {
        &self.value
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
    
    pub const fn is_arithmetic(&self) -> bool {
        matches!(self,
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod
            | Self::BitAnd | Self::BitOr | Self::BitXor | Self::BitNot
            | Self::LeftShift | Self::RightShift | Self::SignedRightShift
        )
    }
    
    pub const fn arithmetic_operator(&self) -> Option<ArithmeticOperator> {
        Some(match self {
            Self::Add => Add,
            Self::Sub => Sub,
            Self::Mul => Mul,
            Self::Div => Div,
            Self::Mod => Mod,
            Self::BitOr => BitOr,
            Self::BitAnd => BitAnd,
            Self::BitXor => BitXor,
            Self::BitNot => BitNot,
            Self::LeftShift => LeftShift,
            Self::RightShift => RightShift,
            Self::SignedRightShift => SignedRightShift,
            _ => return None,
        })
    }
    
    pub const fn comparison_operator(&self) -> Option<ComparisonOperator> {
        Some(match self {
            Self::Equal => Equal,
            Self::NotEqual => NotEqual,
            Self::LessThan => LessThan,
            Self::LessThanOrEqual => LessThanOrEqual,
            Self::GreaterThan => GreaterThan,
            Self::GreaterThanOrEqual => GreaterThanOrEqual,
            Self::BitMask => BitMask,
            Self::Value => BooleanVariable,
            _ => return None,       
        })
    }
    
    pub fn arg_value(&self) -> Option<ArgValue> {
        self.arithmetic_operator().map(ArgValue::from).or_else(|| self.comparison_operator().map(ArgValue::from))
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
    FlagCheck { bank: u8, bit: u8, value: bool },
    FlagSet { bank: u8, bit: u8, value: Expression },
    VarOperation { id: u8, op: Operator, source: Expression },
    ForLoop(Expression),
    Block(Box<Statement>, Vec<Statement>),
    Label(String, Box<Statement>),
    BlankLine, // ugly hack to give me some control over the formatting when building the AST from a decompiled script
}

impl Statement {
    pub fn instruction_name(&self) -> &str {
        match self {
            Self::Instruction(name, _) => name,
            Self::GoTo(_) => "goto",
            Self::FlagCheck { .. } => "ck",
            Self::FlagSet { .. } => "set",
            Self::VarOperation { op, source, .. } => match (source, op) {
                (Expression::Var(_), Operator::Assign) => "copy",
                (_, Operator::Assign) => "save",
                (Expression::Var(_), _) if op.is_arithmetic() => "calc2",
                (_, _) if op.is_arithmetic() => "calc",
                _ => "cmp",
            },
            Self::ForLoop(arg) => match arg {
                Expression::Var(_) => "for2",
                _ => "for",           
            },
            Self::Block(head, _) => head.instruction_name(),
            Self::Label(_, inner) => inner.instruction_name(),
            Self::BlankLine => "",       
        }
    }
    
    pub fn is_block(&self) -> bool {
        match self {
            Self::Block(_, _) => true,
            Self::Label(_, stmt) => stmt.is_block(),
            _ => false,
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GoTo(label) => write!(f, "goto {}", label),
            Self::ForLoop(source) => write!(f, "for {}", source),
            Self::Label(label, stmt) => write!(f, "{}:\n{}", label, stmt),
            Self::FlagCheck { bank, bit, value } => {
                if !*value {
                    // checking for flag to be false
                    write!(f, "!")?;
                }
                write!(f, "flag[{}][{}]", bank, bit)?;
                Ok(())
            }
            Self::FlagSet { bank, bit, value } => write!(f, "flag[{}][{}] = {}", bank, bit, value),
            Self::VarOperation { id, op, source } => {
                match op {
                    Operator::BitNot => write!(f, "~var[{}]", id),
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
            Self::Init => write!(f, "main"),
            Self::Exec(name) => write!(f, "function {}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: FunctionName,
    body: Vec<Statement>,
}

impl Function {
    pub const fn new(name: FunctionName, body: Vec<Statement>) -> Self {
        Self {
            name,
            body,
        }
    }
    
    pub const fn is_init(&self) -> bool {
        matches!(self.name, FunctionName::Init)
    }
    
    pub const fn name(&self) -> Option<&str> {
        match &self.name {
            FunctionName::Init => None,
            FunctionName::Exec(name) => Some(name.as_str()),
        }
    }
    
    pub fn body(&self) -> &[Statement] {
        &self.body
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write_block(f, &self.name, &self.body)
    }
}