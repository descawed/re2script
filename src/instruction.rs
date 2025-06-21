use std::io::{Cursor, Read, Write, Seek};
use std::sync::LazyLock;

use anyhow::{bail, Result};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use residat::re2::{CharacterId, Item, SceType, NUM_CHARACTERS, NUM_INSTRUCTIONS};

pub const ANY_EVENT: u8 = u8::MAX;
pub const NUM_EVENTS: u8 = 14;
pub const NUM_VARIABLES: usize = 36;
pub const NUM_MEMBERS: usize = 44;
pub const NUM_SPEEDS: usize = 12;

// opcodes for instructions that have special handling in the parser
pub const OPCODE_NOP: u8 = 0x00;
pub const OPCODE_EVT_END: u8 = 0x01;
pub const OPCODE_IFEL_CK: u8 = 0x06;
pub const OPCODE_ELSE_CK: u8 = 0x07;
pub const OPCODE_ENDIF: u8 = 0x08;
pub const OPCODE_FOR: u8 = 0x0D;
pub const OPCODE_NEXT: u8 = 0x0E;
pub const OPCODE_WHILE: u8 = 0x0F;
pub const OPCODE_EWHILE: u8 = 0x10;
pub const OPCODE_DO: u8 = 0x11;
pub const OPCODE_EDWHILE: u8 = 0x12;
pub const OPCODE_SWITCH: u8 = 0x13;
pub const OPCODE_CASE: u8 = 0x14;
pub const OPCODE_DEFAULT: u8 = 0x15;
pub const OPCODE_ESWITCH: u8 = 0x16;
pub const OPCODE_GOTO: u8 = 0x17;
pub const OPCODE_BREAK: u8 = 0x1A;
pub const OPCODE_FOR2: u8 = 0x1B;
pub const OPCODE_NOP1E: u8 = 0x1E;
pub const OPCODE_NOP1F: u8 = 0x1F;
pub const OPCODE_NOP20: u8 = 0x20;
pub const OPCODE_CK: u8 = 0x21;
pub const OPCODE_SET: u8 = 0x22;
pub const OPCODE_CMP: u8 = 0x23;
pub const OPCODE_SAVE: u8 = 0x24;
pub const OPCODE_COPY: u8 = 0x25;
pub const OPCODE_CALC: u8 = 0x26;
pub const OPCODE_CALC2: u8 = 0x27;
pub const OPCODE_DIR_CK: u8 = 0x39;
pub const OPCODE_MEMBER_CMP: u8 = 0x3E;
pub const OPCODE_SCE_KEY_CK: u8 = 0x4F;
pub const OPCODE_SCE_TRG_CK: u8 = 0x50;
pub const OPCODE_KEEP_ITEM_CK: u8 = 0x5E;
pub const OPCODE_KEEP_ITEM_CK2: u8 = 0x81;
pub const OPCODE_SCE_EM_POS_CK: u8 = 0x85;
pub const OPCODE_POISON_CK: u8 = 0x86;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
pub enum EntityType {
    Player = 1,
    Ally = 2,
    Npc = 3,
    Object = 4,
    Unknown = 5,
}

// lhs = var, rhs = value
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoPrimitive)]
pub enum ComparisonOperator {
    Equal = 0,
    GreaterThan = 1,
    GreaterThanOrEqual = 2,
    LessThan = 3,
    LessThanOrEqual = 4,
    NotEqual = 5,
    BitMask = 6,
    BooleanVariable = 7,
}

impl ComparisonOperator {
    pub const fn from_exact(value: u8) -> Option<Self> {
        Some(match value {
            0 => Self::Equal,
            1 => Self::GreaterThan,
            2 => Self::GreaterThanOrEqual,
            3 => Self::LessThan,
            4 => Self::LessThanOrEqual,
            5 => Self::NotEqual,
            6 => Self::BitMask,
            7 => Self::BooleanVariable,
            _ => return None,
        })
    }
}

impl From<u8> for ComparisonOperator {
    fn from(value: u8) -> Self {
        // for any unrecognized value, the game returns the value of the variable itself as the
        // result of the comparison
        Self::from_exact(value).unwrap_or(ComparisonOperator::BooleanVariable)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
pub enum ArithmeticOperator {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    Mod = 4,
    BitOr = 5,
    BitAnd = 6,
    BitXor = 7,
    BitNot = 8, // value is ignored
    LeftShift = 9,
    RightShift = 10,
    SignedRightShift = 11,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgType {
    U8,
    U16,
    I16,
    Item,
    EntityType,
    EntityIndex,
    EventIndex,
    SceType,
    CharacterId,
    FunctionIndex,
    ComparisonOperator,
    ArithmeticOperator,
    Bool,
    VariableIndex,
    MemberId,
    SpeedId,
}

impl ArgType {
    pub const fn matches(&self, arg_value: ArgValue) -> bool {
        matches!((self, arg_value),
            (Self::U8, ArgValue::U8(_))
            | (Self::U16, ArgValue::U16(_))
            | (Self::I16, ArgValue::I16(_))
            | (Self::Item, ArgValue::Item(_))
            | (Self::EntityType, ArgValue::EntityType(_))
            | (Self::EntityIndex, ArgValue::EntityIndex(_))
            | (Self::EventIndex, ArgValue::EventIndex(_))
            | (Self::SceType, ArgValue::SceType(_))
            | (Self::CharacterId, ArgValue::CharacterId(_))
            | (Self::FunctionIndex, ArgValue::FunctionIndex(_))
            | (Self::ArithmeticOperator, ArgValue::ArithmeticOperator(_))
            | (Self::ComparisonOperator, ArgValue::ComparisonOperator(_))
            | (Self::Bool, ArgValue::Bool(_))
            | (Self::VariableIndex, ArgValue::VariableIndex(_))
            | (Self::MemberId, ArgValue::MemberId(_))
            | (Self::SpeedId, ArgValue::SpeedId(_))
        )
    }

    pub const fn fallback_type(&self) -> Self {
        match self {
            Self::U8 | Self::EntityType | Self::EntityIndex | Self::EventIndex | Self::SceType
            | Self::CharacterId | Self::FunctionIndex | Self::ComparisonOperator
            | Self::ArithmeticOperator | Self::Bool | Self::VariableIndex
            | Self::MemberId | Self::SpeedId => Self::U8,
            Self::U16 | Self::Item => Self::U16, // FIXME: the size of an item argument actually varies by instruction
            Self::I16 => Self::I16,
        }
    }

    pub const fn size(&self) -> usize {
        match self.fallback_type() {
            Self::U8 => 1,
            Self::U16 | Self::I16 => 2,
            _ => unreachable!(),
        }
    }

    pub const fn is_signed(&self) -> bool {
        matches!(self, Self::I16)
    }

    pub fn make_value<T: Into<i32>>(&self, value: T) -> Option<ArgValue> {
        let value = value.into();
        match self {
            Self::U8 => Some(ArgValue::U8(value as u8)),
            Self::U16 => Some(ArgValue::U16(value as u16)),
            Self::I16 => Some(ArgValue::I16(value as i16)),
            Self::Item => Item::try_from_primitive(value as u16).map(|i| ArgValue::Item(i)).ok(),
            Self::EntityType => EntityType::try_from_primitive(value as u8).map(|e| ArgValue::EntityType(e)).ok(),
            Self::EntityIndex => {
                // FIXME: this check should depend on the entity type, which we don't know here
                (value >= 0 && value < NUM_CHARACTERS as i32).then_some(ArgValue::EntityIndex(value as u8))
            }
            Self::EventIndex => {
                let value = value as u8;
                (value < NUM_EVENTS || value == ANY_EVENT).then_some(ArgValue::EventIndex(value))
            }
            Self::SceType => match SceType::from(value as u8) {
                SceType::Unknown => None,
                sce_type => Some(ArgValue::SceType(sce_type)),
            },
            Self::CharacterId => CharacterId::try_from_primitive(value as u8).map(|c| ArgValue::CharacterId(c)).ok(),
            Self::FunctionIndex => Some(ArgValue::FunctionIndex(value as u8)), // FIXME: check number of functions in current script
            Self::ComparisonOperator => ComparisonOperator::from_exact(value as u8).map(|op| ArgValue::ComparisonOperator(op)),
            Self::ArithmeticOperator => ArithmeticOperator::try_from_primitive(value as u8).map(|op| ArgValue::ArithmeticOperator(op)).ok(),
            Self::Bool => match value {
                0 => Some(ArgValue::Bool(false)),
                1 => Some(ArgValue::Bool(true)),
                _ => None,
            },
            Self::VariableIndex => (value >= 0 && value < NUM_VARIABLES as i32).then_some(ArgValue::VariableIndex(value as u8)),
            Self::MemberId => (value >= 0 && value < NUM_MEMBERS as i32).then_some(ArgValue::MemberId(value as u8)),
            Self::SpeedId => (value >= 0 && value < NUM_SPEEDS as i32).then_some(ArgValue::SpeedId(value as u8)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgValue {
    U8(u8),
    U16(u16),
    I16(i16),
    Item(Item),
    EntityType(EntityType),
    EntityIndex(u8),
    EventIndex(u8),
    SceType(SceType),
    CharacterId(CharacterId),
    FunctionIndex(u8),
    ComparisonOperator(ComparisonOperator),
    ArithmeticOperator(ArithmeticOperator),
    Bool(bool),
    VariableIndex(u8),
    MemberId(u8),
    SpeedId(u8),
}

impl ArgValue {
    pub const fn type_(&self) -> ArgType {
        match self {
            Self::U8(_) => ArgType::U8,
            Self::U16(_) => ArgType::U16,
            Self::I16(_) => ArgType::I16,
            Self::Item(_) => ArgType::Item,
            Self::EntityType(_) => ArgType::EntityType,
            Self::EntityIndex(_) => ArgType::EntityIndex,
            Self::EventIndex(_) => ArgType::EventIndex,
            Self::SceType(_) => ArgType::SceType,
            Self::CharacterId(_) => ArgType::CharacterId,
            Self::FunctionIndex(_) => ArgType::FunctionIndex,
            Self::ComparisonOperator(_) => ArgType::ComparisonOperator,
            Self::ArithmeticOperator(_) => ArgType::ArithmeticOperator,
            Self::Bool(_) => ArgType::Bool,
            Self::VariableIndex(_) => ArgType::VariableIndex,
            Self::MemberId(_) => ArgType::MemberId,
            Self::SpeedId(_) => ArgType::SpeedId,
        }
    }

    pub const fn matches(&self, arg_type: ArgType) -> bool {
        arg_type.matches(*self)
    }
    
    pub const fn is_compatible(&self, arg_type: ArgType) -> bool {
        self.matches(arg_type) || arg_type.fallback_type().matches(*self)
    }
    
    pub fn is_valid(&self) -> bool {
        self.type_().make_value(self.as_int()).is_some()
    }

    pub const fn is_simple_integer(&self) -> bool {
        matches!(self, Self::U8(_) | Self::U16(_) | Self::I16(_))
    }

    pub const fn as_int(&self) -> i32 {
        match self {
            Self::U8(v) => *v as i32,
            Self::I16(v) => *v as i32,
            Self::U16(v) => *v as i32,
            Self::Item(i) => *i as i32,
            Self::EntityType(e) => *e as i32,
            Self::EntityIndex(e) => *e as i32,
            Self::EventIndex(e) => *e as i32,
            Self::SceType(s) => *s as i32,
            Self::CharacterId(c) => *c as i32,
            Self::FunctionIndex(f) => *f as i32,
            Self::ComparisonOperator(op) => *op as i32,
            Self::ArithmeticOperator(op) => *op as i32,
            Self::Bool(b) => *b as i32,
            Self::VariableIndex(i) => *i as i32,
            Self::MemberId(i) => *i as i32,
            Self::SpeedId(i) => *i as i32,
        }
    }
}

impl From<bool> for ArgValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<i16> for ArgValue {
    fn from(value: i16) -> Self {
        Self::I16(value)
    }
}

impl From<u16> for ArgValue {
    fn from(value: u16) -> Self {
        Self::U16(value)
    }
}

impl From<SceType> for ArgValue {
    fn from(value: SceType) -> Self {
        Self::SceType(value)
    }
}

impl From<CharacterId> for ArgValue {
    fn from(value: CharacterId) -> Self {
        Self::CharacterId(value)
    }
}

impl From<ComparisonOperator> for ArgValue {
    fn from(value: ComparisonOperator) -> Self {
        Self::ComparisonOperator(value)
    }
}

impl From<ArithmeticOperator> for ArgValue {
    fn from(value: ArithmeticOperator) -> Self {
        Self::ArithmeticOperator(value)
    }
}

impl From<Item> for ArgValue {
    fn from(value: Item) -> Self {
        Self::Item(value)
    }
}

impl From<EntityType> for ArgValue {
    fn from(value: EntityType) -> Self {
        Self::EntityType(value)
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    name: &'static str,
    type_: ArgType,
    fallback_type: ArgType,
    default: Option<ArgValue>,
    is_keyword_only: bool,
    is_calculated: bool,
}

impl Arg {
    /// Create a required positional argument
    pub const fn new(name: &'static str, type_: ArgType) -> Self {
        Self {
            name,
            type_,
            fallback_type: type_.fallback_type(),
            default: None,
            is_keyword_only: false,
            is_calculated: false,
        }
    }

    /// Create an argument with a default value
    ///
    /// A value for this arg may be passed as either a keyword or positional argument. If no value
    /// is provided, the default value will be used.
    pub const fn default(name: &'static str, default: ArgValue) -> Self {
        Self {
            name,
            type_: default.type_(),
            fallback_type: default.type_().fallback_type(),
            default: Some(default),
            is_keyword_only: false,
            is_calculated: false,
        }
    }

    /// Create an argument with a default value
    ///
    /// A value for this arg may be passed as a keyword argument. If no value is provided, the
    /// default value will be used.
    pub const fn keyword(name: &'static str, default: ArgValue) -> Self {
        Self {
            name,
            type_: default.type_(),
            fallback_type: default.type_().fallback_type(),
            default: Some(default),
            is_keyword_only: true,
            is_calculated: false,
        }
    }

    /// Create an argument whose value is unused
    ///
    /// A value for this arg may be passed by keyword only. If no value is provided, the default
    /// value will be used.
    pub const fn unused(name: &'static str, default: ArgValue) -> Self {
        Self {
            name,
            type_: default.type_(),
            fallback_type: default.type_().fallback_type(),
            default: Some(default),
            is_keyword_only: true,
            is_calculated: false,
        }
    }

    /// Create an argument whose value is calculated
    ///
    /// A value for this arg may be passed by keyword only. If no value is provided, the value will
    /// be calculated according to the rules of the instruction the argument belongs to.
    pub const fn calculated(name: &'static str, type_: ArgType) -> Self {
        Self {
            name,
            type_,
            fallback_type: type_.fallback_type(),
            default: None,
            is_keyword_only: true,
            is_calculated: true,
        }
    }

    /// Create an 8-bit argument whose value is an item (as opposed to the
    pub const fn item8(name: &'static str) -> Self {
        Self {
            name,
            type_: ArgType::Item,
            fallback_type: ArgType::U8,
            default: None,
            is_keyword_only: false,
            is_calculated: false,
        }
    }

    /// Create an 8-bit unused argument which defaults to 0 and is only present for alignment
    pub const fn align() -> Self {
        Self {
            name: "align",
            type_: ArgType::U8,
            fallback_type: ArgType::U8,
            default: Some(ArgValue::U8(0)),
            is_keyword_only: true,
            is_calculated: false,
        }
    }

    /// Create a 16-bit argument representing the size of a block
    ///
    /// This argument is generally calculated automatically
    pub const fn block_size() -> Self {
        Self::calculated("block_size", ArgType::U16)
    }

    /// Create an 8-bit argument with the given name
    pub const fn u8(name: &'static str) -> Self {
        Self::new(name, ArgType::U8)
    }

    /// Create a signed 16-bit argument with the given name
    pub const fn i16(name: &'static str) -> Self {
        Self::new(name, ArgType::I16)
    }

    /// Create an unsigned 16-bit argument with the given name
    pub const fn u16(name: &'static str) -> Self {
        Self::new(name, ArgType::U16)
    }

    /// Create a boolean argument with the given name
    pub const fn bool(name: &'static str) -> Self {
        Self::new(name, ArgType::Bool)
    }

    /// The size of this argument's value in bytes
    pub const fn size(&self) -> usize {
        self.fallback_type.size()
    }

    /// Turn an integer into a value of a type compatible with this argument
    pub fn make_value<T: Into<i32>>(&self, value: T) -> ArgValue {
        let value = value.into();
        self.type_.make_value(value).unwrap_or_else(|| self.fallback_type.make_value(value).unwrap())
    }

    /// Is this arg's value a signed value?
    pub const fn is_signed(&self) -> bool {
        self.type_.is_signed()
    }

    /// Is this argument only allowed to be passed as a keyword argument and not a positional argument?
    pub const fn is_keyword_only(&self) -> bool {
        self.is_keyword_only
    }

    /// Should this argument be shown if it has the given value when decompiling the parent instruction?
    pub fn should_show_value(&self, value: ArgValue) -> bool {
        !self.is_calculated && Some(value) != self.default
    }

    /// Get the name of this argument
    pub const fn name(&self) -> &'static str {
        self.name
    }
    
    /// Write the binary representation of this argument to the provided sink
    pub fn write<T: Write + Seek>(&self, mut f: T, value: ArgValue) -> Result<()> {
        match self.fallback_type {
            ArgType::U8 => {
                let value = value.as_int() as u8;
                f.write_all(&value.to_le_bytes())
            }
            ArgType::I16 => {
                let value = value.as_int() as i16;
                f.write_all(&value.to_le_bytes())
            }
            ArgType::U16 => {
                let value = value.as_int() as u16;
                f.write_all(&value.to_le_bytes())
            }
            _ => unreachable!(),
        }?;
        
        Ok(())
    } 
}

macro_rules! arg {
    ($name:expr, $type_:ident) => {
        Arg::new($name, ArgType::$type_)
    };
}

macro_rules! arg8 {
    ($name:expr) => {
        Arg::u8($name)
    }
}

macro_rules! var {
    ($name:expr) => {
        Arg::new($name, ArgType::VariableIndex)
    }
}

#[derive(Debug, Clone)]
pub struct InstructionDescription {
    opcode: u8,
    name: &'static str,
    args: Vec<Arg>,
}

impl InstructionDescription {
    const fn new(name: &'static str, args: Vec<Arg>) -> Self {
        Self {
            opcode: u8::MAX,
            name,
            args,
        }
    }

    fn one(name: &'static str, arg: Arg) -> Self {
        Self {
            opcode: u8::MAX,
            name,
            args: vec![arg],
        }
    }

    const fn simple(name: &'static str) -> Self {
        Self {
            opcode: u8::MAX,
            name,
            args: Vec::new(),
        }
    }

    fn aligned(name: &'static str) -> Self {
        Self {
            opcode: u8::MAX,
            name,
            args: vec![Arg::align()],
        }
    }

    fn block(name: &'static str) -> Self {
        Self {
            opcode: u8::MAX,
            name,
            args: vec![Arg::align(), Arg::block_size()],
        }
    }

    pub fn size(&self) -> usize {
        // +1 for the opcode
        1 + self.args.iter().map(Arg::size).sum::<usize>()
    }

    pub fn read_args<T: Read + Seek>(&self, mut f: T) -> Result<Vec<ArgValue>> {
        let mut values = Vec::with_capacity(self.args.len());

        for arg in &self.args {
            let value = match arg.size() {
                1 => {
                    let mut buf = [0u8];
                    f.read_exact(&mut buf)?;
                    buf[0] as i32
                }
                2 => {
                    let mut buf = [0u8, 0u8];
                    f.read_exact(&mut buf)?;
                    if arg.is_signed() {
                        i16::from_le_bytes(buf) as i32
                    } else {
                        u16::from_le_bytes(buf) as i32
                    }
                }
                _ => bail!("Only 8-bit and 16-bit arguments are supported; got {}-bit argument", arg.size() * 8),
            };

            values.push(arg.make_value(value));
        }

        Ok(values)
    }
    
    pub const fn name(&self) -> &'static str {
        self.name
    }
}

pub static INSTRUCTION_DESCRIPTIONS: LazyLock<[InstructionDescription; NUM_INSTRUCTIONS]> = LazyLock::new(|| {
    let mut descriptions = [
        InstructionDescription::simple("nop"),
        InstructionDescription::aligned("evt_end"),
        InstructionDescription::simple("evt_next"),
        InstructionDescription::one("evt_chain", Arg::new("scd_id", ArgType::FunctionIndex)),
        InstructionDescription::new(
            "evt_exec",
            vec![Arg::new("event", ArgType::EventIndex), Arg::unused("go_sub", ArgValue::U8(0x18)), Arg::new("scd_id", ArgType::FunctionIndex)],
        ),
        InstructionDescription::one("evt_kill", Arg::new("event", ArgType::EventIndex)),
        InstructionDescription::block("ifel_ck"),
        InstructionDescription::block("else_ck"),
        InstructionDescription::aligned("endif"),
        InstructionDescription::simple("sleep"),
        InstructionDescription::one("sleeping", Arg::new("count", ArgType::U16)),
        InstructionDescription::simple("wsleep"),
        InstructionDescription::simple("wsleeping"),
        InstructionDescription::new("for", vec![Arg::align(), Arg::block_size(), Arg::new("count", ArgType::U16)]),
        InstructionDescription::aligned("next"),
        InstructionDescription::new("while", vec![Arg::calculated("condition_size", ArgType::U8), Arg::block_size()]),
        InstructionDescription::aligned("ewhile"),
        InstructionDescription::block("do"),
        InstructionDescription::one("edwhile", Arg::calculated("condition_size", ArgType::U8)),
        // TODO: make a dedicated arg type for variable and flag IDs once I figure out how many of them there are
        InstructionDescription::new("switch", vec![var!("var"), Arg::block_size()]),
        InstructionDescription::new("case", vec![Arg::align(), Arg::block_size(), Arg::new("case_value", ArgType::U16)]),
        InstructionDescription::aligned("default"),
        InstructionDescription::aligned("eswitch"),
        InstructionDescription::new(
            "goto",
            vec![Arg::calculated("ifel_ctr", ArgType::U8), Arg::calculated("loop_ctr", ArgType::U8), Arg::align(), Arg::new("offset", ArgType::I16)],
        ),
        InstructionDescription::one("gosub", Arg::new("sub", ArgType::FunctionIndex)),
        InstructionDescription::aligned("return"),
        InstructionDescription::aligned("break"),
        InstructionDescription::new("for2", vec![Arg::align(), Arg::block_size(), Arg::align(), var!("count")]),
        InstructionDescription::simple("break_point"),
        InstructionDescription::new("work_copy", vec![var!("source"), arg!("destination", MemberId), Arg::bool("is_word")]),
        InstructionDescription::simple("nop"),
        InstructionDescription::simple("nop"),
        InstructionDescription::simple("nop"),
        InstructionDescription::new("ck", vec![Arg::new("bank", ArgType::U8), Arg::new("bit", ArgType::U8), Arg::bool("value")]),
        InstructionDescription::new("set", vec![Arg::new("bank", ArgType::U8), Arg::new("bit", ArgType::U8), Arg::bool("value")]),
        InstructionDescription::new("cmp", vec![Arg::align(), var!("lhs"), Arg::new("op", ArgType::ComparisonOperator), Arg::new("rhs", ArgType::I16)]),
        InstructionDescription::new("save", vec![var!("lhs"), Arg::new("rhs", ArgType::I16)]),
        InstructionDescription::new("copy", vec![var!("lhs"), var!("rhs")]),
        InstructionDescription::new(
            "calc",
            vec![Arg::align(), Arg::new("op", ArgType::ArithmeticOperator), var!("lhs"), Arg::new("rhs", ArgType::I16)],
        ),
        InstructionDescription::new(
            "calc2",
            vec![Arg::align(), Arg::new("op", ArgType::ArithmeticOperator), var!("lhs"), var!("rhs")],
        ),
        InstructionDescription::simple("sce_rnd"),
        InstructionDescription::one("cut_chg", Arg::new("cut", ArgType::U8)),
        InstructionDescription::simple("cut_old"),
        InstructionDescription::new("message_on", vec![Arg::align(), Arg::new("type", ArgType::U8), Arg::new("message", ArgType::U8), Arg::new("time", ArgType::U16)]),
        InstructionDescription::new(
            "aot_set",
            vec![
                arg8!("aot"),
                arg!("sce", SceType),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x"),
                Arg::i16("z"),
                Arg::u16("w"),
                Arg::u16("h"),
                Arg::unused("data0", ArgValue::U16(0x00FF)),
                arg8!("data1"),
                arg!("scd_id", FunctionIndex),
                Arg::u16("data2"),
            ],
        ),
        InstructionDescription::new(
            "obj_model_set",
            vec![
                arg8!("md1"),
                arg8!("id"),
                arg8!("ccol_old"),
                arg8!("ccol_no"),
                arg8!("ctex_old"),
                arg8!("floor"),
                arg8!("super"),
                Arg::u16("type"),
                Arg::u16("be_flag"),
                Arg::i16("attribute"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("dir_x"),
                Arg::i16("dir_y"),
                Arg::i16("dir_z"),
                Arg::i16("atari_offset_x"),
                Arg::i16("atari_offset_y"),
                Arg::i16("atari_offset_z"),
                Arg::i16("atari_size_x"),
                Arg::i16("atari_size_y"),
                Arg::i16("atari_size_z"),
            ],
        ),
        InstructionDescription::new("work_set", vec![arg!("type", EntityType), arg!("id", EntityIndex)]),
        InstructionDescription::new("speed_set", vec![arg!("id", SpeedId), Arg::i16("value")]),
        InstructionDescription::simple("add_speed"),
        InstructionDescription::simple("add_aspeed"),
        InstructionDescription::new(
            "pos_set",
            vec![Arg::align(), Arg::i16("x"), Arg::i16("y"), Arg::i16("z")],
        ),
        InstructionDescription::new(
            "dir_set",
            vec![Arg::align(), Arg::i16("x"), Arg::i16("y"), Arg::i16("z")],
        ),
        InstructionDescription::new("member_set", vec![arg!("member", MemberId), Arg::i16("value")]),
        InstructionDescription::new("member_set2", vec![arg!("destination", MemberId), var!("source")]),
        InstructionDescription::new(
            "se_on",
            vec![arg8!("vab"), Arg::i16("edt"), Arg::i16("data0"), Arg::i16("x"), Arg::i16("y"), Arg::i16("z")],
        ),
        InstructionDescription::new("sca_id_set", vec![arg8!("i_entry"), Arg::u16("id")]),
        InstructionDescription::new("flr_set", vec![arg8!("floor"), arg8!("value")]),
        InstructionDescription::new("dir_ck", vec![Arg::align(), Arg::i16("x"), Arg::i16("z"), Arg::i16("add")]),
        InstructionDescription::new(
            "sce_espr_on",
            vec![
                Arg::align(),
                Arg::u16("data0"),
                Arg::u16("data1"),
                Arg::u16("data2"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("dir_y"),
            ],
        ),
        InstructionDescription::new(
            "door_aot_set",
            vec![
                arg8!("aot"),
                Arg::keyword("sce", ArgValue::SceType(SceType::Door)),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x"),
                Arg::i16("z"),
                Arg::u16("w"),
                Arg::u16("h"),
                Arg::i16("next_pos_x"),
                Arg::i16("next_pos_y"),
                Arg::i16("next_pos_z"),
                Arg::i16("next_cdir_y"),
                arg8!("next_stage"),
                arg8!("next_room"),
                arg8!("next_cut"),
                arg8!("next_floor"),
                arg8!("dtex_type"),
                arg8!("door_type"),
                arg8!("knock_type"),
                arg8!("key_id"),
                Arg::item8("key_type"),
                Arg::unused("free", ArgValue::U8(0)),
            ],
        ),
        InstructionDescription::one("cut_auto", Arg::bool("on_off")),
        InstructionDescription::new("member_copy", vec![var!("dest"), arg!("source", MemberId)]),
        InstructionDescription::new(
            "member_cmp",
            vec![Arg::align(), arg!("member", MemberId), Arg::new("op", ArgType::ComparisonOperator), Arg::i16("value")],
        ),
        InstructionDescription::new("plc_motion", vec![arg8!("motion_id"), arg8!("mode"), arg8!("param")]),
        InstructionDescription::new(
            "plc_dest",
            vec![Arg::align(), arg8!("animation"), arg8!("bit"), Arg::i16("x"), Arg::i16("z")],
        ),
        InstructionDescription::new(
            "plc_neck",
            vec![arg8!("op"), Arg::i16("x"), Arg::i16("y"), Arg::i16("z"), arg8!("speed_x"), arg8!("speed_z")],
        ),
        InstructionDescription::simple("plc_ret"),
        InstructionDescription::new("plc_flg", vec![Arg::align(), arg8!("data0"), arg8!("data1")]),
        InstructionDescription::new(
            "sce_em_set",
            vec![
                Arg::align(),
                arg8!("em_no"),
                arg!("id", CharacterId),
                Arg::u16("type"),
                arg8!("floor"),
                arg8!("sound_flg"),
                arg8!("model_type"),
                arg8!("em_set_flg"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("cdir_y"),
                Arg::i16("motion"),
                Arg::i16("ctr_flg"),
            ],
        ),
        InstructionDescription::new("col_chg_set", vec![arg8!("data0"), arg8!("data1"), arg8!("data2"), arg8!("data3")]),
        InstructionDescription::new(
            "aot_reset",
            vec![arg8!("aot"), arg!("sce", SceType), arg8!("sat"), Arg::u16("data0"), Arg::u16("data1"), Arg::u16("data2")],
        ),
        InstructionDescription::one("aot_on", arg8!("aot")),
        InstructionDescription::new(
            "super_set",
            vec![
                Arg::align(),
                arg8!("work"),
                arg8!("id"),
                Arg::i16("px"),
                Arg::i16("py"),
                Arg::i16("pz"),
                Arg::i16("dx"),
                Arg::i16("dy"),
                Arg::i16("dz"),
            ],
        ),
        InstructionDescription::new("super_reset", vec![Arg::align(), Arg::i16("dx"), Arg::i16("dy"), Arg::i16("dz")]),
        InstructionDescription::one("plc_gun", arg8!("id")),
        InstructionDescription::new("cut_replace", vec![arg8!("id"), arg8!("value")]),
        InstructionDescription::new("sce_espr_kill", vec![arg8!("id"), arg8!("tp"), arg8!("work_kind"), arg8!("work_no")]),
        InstructionDescription::new(
            "door_model_set",
            vec![
                arg8!("data0"),
                arg8!("id"),
                arg8!("ofs_y"),
                arg8!("be_flg"),
                arg8!("data5"),
                Arg::u16("data6"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("dir_y"),
                Arg::u16("data10"),
                Arg::u16("data11"),
                Arg::u16("data12"),
            ],
        ),
        InstructionDescription::new(
            "item_aot_set",
            vec![
                arg8!("aot"),
                Arg::keyword("sce", ArgValue::SceType(SceType::Item)),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x"),
                Arg::i16("z"),
                Arg::u16("w"),
                Arg::u16("h"),
                arg!("i_item", Item),
                Arg::u16("n_item"),
                Arg::u16("flag"),
                arg8!("md1"),
                arg8!("action"),
            ],
        ),
        InstructionDescription::new("sce_key_ck", vec![Arg::bool("on_off"), Arg::u16("keys")]),
        InstructionDescription::new("sce_trg_ck", vec![Arg::bool("on_off"), Arg::u16("keys")]),
        InstructionDescription::new(
            "sce_bgm_control",
            vec![arg8!("id"), arg8!("op"), arg8!("type"), arg8!("vol_l"), arg8!("vol_r")],
        ),
        InstructionDescription::new(
            "sce_espr_control",
            vec![arg8!("id"), arg8!("type"), arg8!("return"), arg8!("work_kind"), arg8!("work_no")],
        ),
        InstructionDescription::new("sce_fade_set", vec![arg8!("data0"), arg8!("data1"), arg8!("data2"), Arg::u16("data3")]),
        InstructionDescription::new(
            "sce_espr3d_on",
            vec![
                Arg::align(),
                Arg::u16("data0"),
                Arg::u16("data1"),
                Arg::u16("data2"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("dir_x"),
                Arg::i16("dir_y"),
                Arg::i16("dir_z"),
                Arg::i16("data3"),
            ],
        ),
        InstructionDescription::new(
            "member_calc",
            vec![Arg::align(), arg!("op", ArithmeticOperator), arg!("member", MemberId), Arg::i16("value")],
        ),
        InstructionDescription::new(
            "member_calc2",
            vec![arg!("op", ArithmeticOperator), arg!("member", MemberId), var!("var")],
        ),
        InstructionDescription::new(
            "sce_bgmtbl_set",
            vec![Arg::align(), arg8!("stage"), arg8!("room"), Arg::u16("data1"), Arg::u16("data2")],
        ),
        InstructionDescription::new("plc_rot", vec![arg8!("id"), Arg::u16("sce_free0")]),
        InstructionDescription::new(
            "xa_on",
            vec![arg8!("mode"), Arg::u16("number")],
        ),
        InstructionDescription::one("weapon_chg", Arg::item8("weapon")),
        InstructionDescription::one("plc_cnt", arg8!("id")),
        InstructionDescription::new("sce_shake_on", vec![arg8!("slide_ofs"), arg8!("copy_ofs")]),
        InstructionDescription::one("mizu_div_set", arg8!("id")),
        InstructionDescription::one("keep_item_ck", Arg::item8("item")),
        InstructionDescription::one("xa_vol", arg8!("volume")),
        InstructionDescription::new(
            "kage_set",
            vec![
                arg8!("work"),
                arg8!("id"),
                arg8!("data0"),
                arg8!("data1"),
                arg8!("data2"),
                Arg::u16("data3"),
                Arg::u16("data4"),
                Arg::u16("data5"),
                Arg::u16("data6"),
            ],
        ),
        InstructionDescription::new("cut_be_set", vec![arg8!("id"), arg8!("value"), Arg::bool("on_off")]),
        InstructionDescription::one("sce_item_lost", Arg::item8("item")),
        InstructionDescription::simple("plc_gun_eff"),
        InstructionDescription::new(
            "sce_espr_on2",
            vec![
                arg8!("dir_y_id2"),
                Arg::u16("data1"),
                arg8!("work_kind"),
                arg8!("work_no"),
                Arg::u16("data3"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("dir_y"),
            ],
        ),
        InstructionDescription::one("sce_espr_kill2", arg8!("id")),
        InstructionDescription::simple("plc_stop"),
        InstructionDescription::new(
            "aot_set_4p",
            vec![
                arg8!("aot"),
                arg!("sce", SceType),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x0"),
                Arg::i16("z0"),
                Arg::i16("x1"),
                Arg::i16("z1"),
                Arg::i16("x2"),
                Arg::i16("z2"),
                Arg::i16("x3"),
                Arg::i16("z3"),
                Arg::u16("data0"),
                Arg::u16("data1"),
                Arg::u16("data2"),
            ],
        ),
        InstructionDescription::new(
            "door_aot_set_4p",
            vec![
                arg8!("aot"),
                Arg::keyword("sce", ArgValue::SceType(SceType::Door)),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x0"),
                Arg::i16("z0"),
                Arg::i16("x1"),
                Arg::i16("z1"),
                Arg::i16("x2"),
                Arg::i16("z2"),
                Arg::i16("x3"),
                Arg::i16("z3"),
                Arg::i16("next_pos_x"),
                Arg::i16("next_pos_y"),
                Arg::i16("next_pos_z"),
                Arg::i16("next_cdir_y"),
                arg8!("next_stage"),
                arg8!("next_room"),
                arg8!("next_cut"),
                arg8!("next_floor"),
                arg8!("dtex_type"),
                arg8!("door_type"),
                arg8!("knock_type"),
                arg8!("key_id"),
                Arg::item8("key_type"),
                Arg::unused("free", ArgValue::U8(0)),
            ],
        ),
        InstructionDescription::new(
            "item_aot_set_4p",
            vec![
                arg8!("aot"),
                Arg::keyword("sce", ArgValue::SceType(SceType::Item)),  
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x0"),
                Arg::i16("z0"),
                Arg::i16("x1"),
                Arg::i16("z1"),
                Arg::i16("x2"),
                Arg::i16("z2"),
                Arg::i16("x3"),
                Arg::i16("z3"),
                arg!("i_item", Item),
                Arg::u16("n_item"),
                Arg::u16("flag"),
                arg8!("md1"),
                arg8!("action"),
            ],
        ),
        InstructionDescription::new(
            "light_pos_set",
            vec![Arg::align(), arg8!("index"), arg8!("xyz"), Arg::i16("position")],
        ),
        InstructionDescription::new(
            "light_kido_set",
            vec![arg8!("index"), Arg::i16("luminosity")],
        ),
        InstructionDescription::simple("rbj_reset"),
        InstructionDescription::new("sce_scr_move", vec![Arg::align(), Arg::i16("scrl_y")]),
        InstructionDescription::new(
            "parts_set",
            vec![Arg::align(), arg8!("id"), arg8!("type"), Arg::i16("value")],
        ),
        InstructionDescription::one("movie_on", arg8!("id")),
        InstructionDescription::simple("splc_ret"),
        InstructionDescription::simple("splc_sce"),
        InstructionDescription::new(
            "super_on",
            vec![Arg::align(), arg8!("data0"), arg8!("data1"), Arg::i16("data2"), Arg::i16("data3"), Arg::i16("data4"), Arg::i16("data5"), Arg::i16("data6"), Arg::i16("data7")],
        ),
        InstructionDescription::new(
            "mirror_set",
            vec![arg8!("flag"), Arg::u16("position"), Arg::u16("min"), Arg::u16("max")],
        ),
        InstructionDescription::new("sce_fade_adjust", vec![arg8!("data0"), Arg::i16("data1")]),
        InstructionDescription::new(
            "sce_espr3d_on2",
            vec![
                arg8!("dir_y_id2"),
                Arg::u16("bit"),
                Arg::u16("data4"),
                Arg::u16("data6"),
                Arg::u16("data8"),
                Arg::u16("dataA"),
                Arg::u16("dataC"),
                Arg::u16("dataE"),
                Arg::u16("data10"),
                Arg::u16("data12"),
                Arg::u16("data14"),
            ],
        ),
        InstructionDescription::new("sce_item_get", vec![Arg::item8("item"), arg8!("count")]),
        InstructionDescription::new("sce_line_start", vec![arg8!("id"), Arg::u16("value")]),
        InstructionDescription::new("sce_line_main", vec![arg8!("id"), Arg::i16("data0"), Arg::i16("data1")]),
        InstructionDescription::simple("sce_line_end"),
        InstructionDescription::new(
            "sce_parts_bomb",
            vec![
                Arg::align(),
                arg8!("data2"),
                arg8!("data3"),
                arg8!("data4"),
                arg8!("data5"),
                Arg::i16("data6"),
                Arg::i16("data8"),
                Arg::i16("dataA"),
                Arg::i16("dataC"),
                Arg::i16("dataE"),
            ],
        ),
        InstructionDescription::new(
            "sce_parts_down",
            vec![
                arg8!("id"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("c_dir_z"),
                Arg::i16("dir_x"),
                Arg::i16("dir_y"),
                Arg::i16("dir_z"),
            ],
        ),
        InstructionDescription::new(
            "light_color_set",
            vec![arg8!("index"), arg8!("r"), arg8!("g"), arg8!("b"), Arg::align()],
        ),
        InstructionDescription::new(
            "light_pos_set2",
            vec![arg8!("n_cut"), arg8!("index"), arg8!("xyz"), Arg::i16("position")],
        ),
        InstructionDescription::new(
            "light_kido_set2",
            vec![Arg::align(), arg8!("n_cut"), arg8!("index"), Arg::u16("luminosity")],
        ),
        InstructionDescription::new(
            "light_color_set2",
            vec![arg8!("n_cut"), arg8!("index"), arg8!("r"), arg8!("g"), arg8!("b")],
        ),
        InstructionDescription::one("se_vol", arg8!("volume")),
        InstructionDescription::new("keep_item_ck2", vec![Arg::item8("item"), arg8!("count")]),
        InstructionDescription::new("sce_espr_task", vec![arg8!("work_kind"), arg8!("work_no")]),
        InstructionDescription::simple("plc_heal"),
        InstructionDescription::one("st_map_hint", arg8!("dr_mode_tag")),
        InstructionDescription::new(
            "sce_em_pos_ck",
            vec![arg8!("id"), arg8!("data1"), arg8!("att"), Arg::u16("flg")],
        ),
        InstructionDescription::simple("poison_ck"),
        InstructionDescription::simple("poison_clr"),
        InstructionDescription::new("sce_item_lost2", vec![Arg::item8("item"), arg8!("count")]),
        InstructionDescription::simple("evt_next2"),
        InstructionDescription::new("vib_set0", vec![Arg::align(), Arg::u16("data0"), Arg::u16("data1")]),
        InstructionDescription::new("vib_set1", vec![arg8!("id"), Arg::u16("value1"), Arg::u16("value2")]),
        InstructionDescription::new(
            "vib_fade_set",
            vec![Arg::align(), arg8!("data0"), arg8!("data1"), Arg::u16("data2"), Arg::u16("data3")],
        ),
        InstructionDescription::new(
            "item_aot_set2",
            vec![
                arg8!("aot"),
                Arg::keyword("sce", ArgValue::SceType(SceType::Item)),
                arg8!("sat"),
                arg8!("floor"),
                arg8!("super"),
                Arg::i16("x"),
                Arg::i16("z"),
                Arg::u16("w"),
                Arg::u16("h"),
                arg!("i_item", Item),
                Arg::u16("n_item"),
                Arg::u16("flag"),
                arg8!("md1"),
                arg8!("action"),
                arg8!("data16"),
                arg8!("data17"),
            ],
        ),
        InstructionDescription::new(
            "sce_em_set2",
            vec![
                Arg::align(),
                arg8!("em_no"),
                arg!("id", CharacterId),
                Arg::u16("type"),
                arg8!("floor"),
                arg8!("sound_flg"),
                arg8!("model_type"),
                arg8!("em_set_flg"),
                Arg::i16("x"),
                Arg::i16("y"),
                Arg::i16("z"),
                Arg::i16("cdir_y"),
                Arg::u16("timer0"),
                Arg::u16("timer1"),
                Arg::u16("data16"),
            ],
        ),
    ];

    // initialize opcodes
    for (i, description) in descriptions.iter_mut().enumerate() {
        description.opcode = i as u8;
    }

    descriptions
});

#[derive(Debug, Clone)]
pub enum MixedArgValue {
    KnownType(ArgValue),
    UnknownType(i32),
}

impl MixedArgValue {
    pub fn resolve(&self, info: &Arg) -> ArgValue {
        match self {
            Self::KnownType(value) => *value,
            Self::UnknownType(value) => info.make_value(*value),
        }
    }
    
    pub fn as_int(&self) -> i32 {
        match self {
            Self::KnownType(value) => value.as_int(),
            Self::UnknownType(value) => *value,
        }
    }
}

impl From<ArgValue> for MixedArgValue {
    fn from(value: ArgValue) -> Self {
        Self::KnownType(value)
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    description: &'static InstructionDescription,
    arg_values: Vec<ArgValue>,
    offset: usize,
}

impl Instruction {
    pub fn make_from_opcode(opcode: u8, offset: usize, positional_arguments: &[MixedArgValue], keyword_arguments: &[(&str, MixedArgValue)]) -> Result<Self> {
        let opcode = opcode as usize;
        if opcode >= INSTRUCTION_DESCRIPTIONS.len() {
            bail!("Invalid opcode: {:02X}", opcode);
        }
        
        let description = &INSTRUCTION_DESCRIPTIONS[opcode];
        let mut arg_values = Vec::with_capacity(description.args.len());
        let mut positional_index = 0usize;
        for arg_info in &description.args {
            let mut found = false;
            
            for (name, value) in keyword_arguments {
                if *name == arg_info.name {
                    let arg_value = value.resolve(arg_info);
                    if !arg_value.is_compatible(arg_info.type_) {
                        bail!("Argument {} of instruction {} is a {:?}-type argument and is not compatible with a value of {:?}", arg_info.name, description.name, arg_info.type_, arg_value);
                    }
                    arg_values.push(arg_value);
                    found = true;
                    break;
                }
            }
            
            if found {
                continue;
            }
            
            if !arg_info.is_keyword_only {
                let value = positional_arguments[positional_index].resolve(arg_info);
                if !value.is_compatible(arg_info.type_) {
                    bail!("Argument {} of instruction {} is a {:?}-type argument and is not compatible with a value of {:?}", arg_info.name, description.name, arg_info.type_, value);
                }
                arg_values.push(value);
                positional_index += 1;
                continue;
            }
            
            if let Some(default_value) = arg_info.default {
                arg_values.push(default_value);
                continue;
            }
            
            bail!("No value provided for argument {} of instruction {}", arg_info.name, description.name);
        }
        
        Ok(Self {
            description,
            arg_values,
            offset,
        })
    }

    pub fn make_from_name(name: &str, offset: usize, positional_arguments: &[MixedArgValue], keyword_arguments: &[(&str, MixedArgValue)]) -> Result<Self> {
        for description in &*INSTRUCTION_DESCRIPTIONS {
            if description.name == name {
                return Self::make_from_opcode(description.opcode, offset, positional_arguments, keyword_arguments);
            }
        }
        
        bail!("Unknown instruction: {}", name)
    }
    
    pub fn read<T: Read + Seek>(mut f: T) -> Result<Self> {
        let offset = f.stream_position()? as usize;
        
        let mut opcode = [0u8];
        f.read_exact(&mut opcode)?;

        let opcode = opcode[0] as usize;
        let descriptions = &*INSTRUCTION_DESCRIPTIONS;
        if opcode >= descriptions.len() {
            bail!("Invalid opcode {opcode}");
        }

        let description = &descriptions[opcode];
        let arg_values = description.read_args(&mut f)?;

        Ok(Self {
            description,
            arg_values,
            offset,
        })
    }
    
    pub fn write<T: Write + Seek>(&self, mut f: T) -> Result<()> {
        f.write_all(&self.description.opcode.to_le_bytes())?;
        
        for (arg_info, arg_value) in self.description.args.iter().zip(self.arg_values.iter()) {
            arg_info.write(&mut f, *arg_value)?;
        }
        
        Ok(())
    }

    pub fn read_function(buf: &[u8]) -> Vec<Self> {
        let size = buf.len() as u64;
        let mut cursor = Cursor::new(buf);

        let mut instructions = Vec::new();
        while cursor.position() < size {
            let Ok(instruction) = Self::read(&mut cursor) else {
                // for now, don't propagate the error, just return what we have
                break;
            };
            instructions.push(instruction);
        }

        instructions
    }

    pub fn read_script(buf: &[u8]) -> Result<Vec<Vec<Self>>> {
        let size = buf.len();
        let mut word = [0u8, 0u8];
        let mut cursor = Cursor::new(buf);
        cursor.read_exact(&mut word)?;

        let header_size = u16::from_le_bytes(word) as usize;
        let num_functions = header_size >> 1;
        let mut offsets = Vec::with_capacity(num_functions + 1);
        offsets.push(header_size);
        for _ in 1..num_functions {
            let mut word = [0u8, 0u8];
            cursor.read_exact(&mut word)?;
            let offset = u16::from_le_bytes(word);
            offsets.push(offset as usize);
        }
        offsets.push(size);

        let mut functions = Vec::with_capacity(num_functions);
        for window in offsets.windows(2) {
            let offset = window[0];
            let next_offset = window[1];
            let func_buf = &buf[offset..next_offset];
            let mut function = Self::read_function(func_buf);
            // make offsets absolute
            for instruction in &mut function {
                instruction.offset += offset;
            }
            functions.push(function);
        }

        Ok(functions)
    }

    pub fn size(&self) -> usize {
        self.description.size()
    }

    pub const fn is_block_start(&self) -> bool {
        matches!(
            self.description.opcode,
            OPCODE_IFEL_CK | OPCODE_ELSE_CK | OPCODE_SWITCH | OPCODE_CASE
            | OPCODE_WHILE | OPCODE_DO | OPCODE_FOR | OPCODE_FOR2
        )
    }

    pub const fn is_flag_op(&self) -> bool {
        matches!(self.description.opcode, OPCODE_CK | OPCODE_SET)
    }

    pub const fn is_var_op(&self) -> bool {
        matches!(self.description.opcode, OPCODE_CALC | OPCODE_CALC2 | OPCODE_CMP | OPCODE_SAVE | OPCODE_COPY)
    }

    pub const fn is_for_loop(&self) -> bool {
        matches!(self.description.opcode, OPCODE_FOR | OPCODE_FOR2)
    }

    pub const fn is_nop(&self) -> bool {
        matches!(self.description.opcode, OPCODE_NOP | OPCODE_NOP1E | OPCODE_NOP1F | OPCODE_NOP20)
    }

    pub const fn has_conditions(&self) -> bool {
        matches!(self.description.opcode, OPCODE_IFEL_CK | OPCODE_WHILE | OPCODE_EDWHILE)
    }

    pub const fn is_condition(&self) -> bool {
        matches!(
            self.description.opcode,
            OPCODE_CK | OPCODE_CMP | OPCODE_DIR_CK | OPCODE_SCE_KEY_CK | OPCODE_SCE_TRG_CK
            | OPCODE_KEEP_ITEM_CK | OPCODE_KEEP_ITEM_CK2 | OPCODE_SCE_EM_POS_CK | OPCODE_POISON_CK
            | OPCODE_MEMBER_CMP
        )
    }

    pub const fn opcode(&self) -> u8 {
        self.description.opcode
    }
    
    pub const fn offset(&self) -> usize {
        self.offset
    }

    pub fn args<const N: usize>(&self, names: [&str; N]) -> [Option<ArgValue>; N] {
        let mut values = [const { None }; N];

        for (i, arg) in self.description.args.iter().enumerate() {
            for (j, name) in names.iter().enumerate() {
                if arg.name == *name {
                    values[j] = Some(self.arg_values[i]);
                    if values.iter().all(Option::is_some) {
                        return values;
                    }
                }
            }
        }

        values
    }

    pub fn arg(&self, name: &str) -> Option<ArgValue> {
        self.args([name])[0]
    }
    
    pub fn set_args(&mut self, values: &[(&str, ArgValue)]) -> Result<()> {
        let mut num_found = 0;
        
        for (i, arg) in self.description.args.iter().enumerate() {
            for (name, value) in values {
                if arg.name == *name {
                    if !value.is_compatible(arg.type_) {
                        bail!("Argument {} of instruction {} is a {:?}-type argument and is not compatible with a value of {:?}", arg.name, self.description.name, arg.type_, value);
                    }
                    
                    self.arg_values[i] = *value;
                    num_found += 1;
                    if num_found == values.len() {
                        return Ok(());
                    }
                }
            }
        }

        if num_found != values.len() {
            bail!("Did not recognize all arguments provided to instruction {}: {:?}", self.description.name, values);
        } else {
            Ok(())
        }
    }
    
    pub fn set_arg(&mut self, name: &str, value: ArgValue) -> Result<()> {
        self.set_args(&[(&name, value)])
    }

    pub fn arg_values(&self) -> &[ArgValue] {
        &self.arg_values
    }

    pub fn describe_args(&self) -> impl Iterator<Item=(&Arg, &ArgValue)> {
        self.description.args.iter().zip(&self.arg_values)
    }

    pub const fn name(&self) -> &'static str {
        self.description.name
    }
}