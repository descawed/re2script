use std::collections::HashMap;
use std::sync::LazyLock;

use residat::re2::{SceType, SAT_TRIGGER_CENTER, SAT_4P, SAT_TRIGGER_BY_ALLY, SAT_TRIGGER_BY_NPC, SAT_TRIGGER_BY_OBJECT, SAT_TRIGGER_BY_PLAYER, SAT_TRIGGER_FRONT, SAT_TRIGGER_ON_ACTION};

use crate::instruction::{ArgValue, ArithmeticOperator, ComparisonOperator, EntityType, ANY_EVENT};

pub static SCRIPT_CONSTANTS: LazyLock<HashMap<&'static str, ArgValue>> = LazyLock::new(|| {
    let mut constants = HashMap::new();

    constants.insert("true", true.into());
    constants.insert("false", false.into());

    constants.insert("player", EntityType::Player.into());
    constants.insert("ally", EntityType::Ally.into());
    constants.insert("npc", EntityType::Npc.into());
    constants.insert("object", EntityType::Object.into());

    constants.insert("eq", ComparisonOperator::Equal.into());
    constants.insert("ne", ComparisonOperator::NotEqual.into());
    constants.insert("lt", ComparisonOperator::LessThan.into());
    constants.insert("le", ComparisonOperator::LessThanOrEqual.into());
    constants.insert("gt", ComparisonOperator::GreaterThan.into());
    constants.insert("ge", ComparisonOperator::GreaterThanOrEqual.into());
    constants.insert("mask", ComparisonOperator::BitMask.into());
    constants.insert("value", ComparisonOperator::BooleanVariable.into());
    
    constants.insert("add", ArithmeticOperator::Add.into());
    constants.insert("sub", ArithmeticOperator::Sub.into());
    constants.insert("mul", ArithmeticOperator::Mul.into());
    constants.insert("div", ArithmeticOperator::Div.into());
    constants.insert("mod", ArithmeticOperator::Mod.into());
    constants.insert("bit_or", ArithmeticOperator::BitOr.into());
    constants.insert("bit_and", ArithmeticOperator::BitAnd.into());
    constants.insert("bit_xor", ArithmeticOperator::BitXor.into());
    constants.insert("bit_not", ArithmeticOperator::BitNot.into());
    constants.insert("shl", ArithmeticOperator::LeftShift.into());
    constants.insert("shr", ArithmeticOperator::RightShift.into());
    constants.insert("sar", ArithmeticOperator::SignedRightShift.into());
    
    constants.insert("auto", SceType::Auto.into());
    constants.insert("door", SceType::Door.into());
    constants.insert("item", SceType::Item.into());
    constants.insert("normal", SceType::Normal.into());
    constants.insert("message", SceType::Message.into());
    constants.insert("event", SceType::Event.into());
    constants.insert("flag_chg", SceType::FlagChg.into());
    constants.insert("water", SceType::Water.into());
    constants.insert("move", SceType::Move.into());
    constants.insert("save", SceType::Save.into());
    constants.insert("item_box", SceType::ItemBox.into());
    constants.insert("damage", SceType::Damage.into());
    constants.insert("status", SceType::Status.into());
    constants.insert("hikidashi", SceType::Hikidashi.into());
    constants.insert("windows", SceType::Windows.into());
    
    constants.insert("any", ArgValue::EventIndex(ANY_EVENT));
    
    constants.insert("SAT_4P", ArgValue::U8(SAT_4P));
    constants.insert("SAT_TRIGGER_CENTER", ArgValue::U8(SAT_TRIGGER_CENTER));
    constants.insert("SAT_TRIGGER_BY_PLAYER", ArgValue::U8(SAT_TRIGGER_BY_PLAYER));
    constants.insert("SAT_TRIGGER_BY_ALLY", ArgValue::U8(SAT_TRIGGER_BY_ALLY));
    constants.insert("SAT_TRIGGER_BY_NPC", ArgValue::U8(SAT_TRIGGER_BY_NPC));
    constants.insert("SAT_TRIGGER_BY_OBJECT", ArgValue::U8(SAT_TRIGGER_BY_OBJECT));
    constants.insert("SAT_TRIGGER_FRONT", ArgValue::U8(SAT_TRIGGER_FRONT));
    constants.insert("SAT_TRIGGER_ON_ACTION", ArgValue::U8(SAT_TRIGGER_ON_ACTION));

    constants
});

pub fn search_constant(search_value: ArgValue) -> Option<&'static str> {
    if search_value.is_simple_integer() {
        return None; // we're likely to get false positives for primitive types
    }
    
    let constants = &*SCRIPT_CONSTANTS;
    for (name, constant_value) in constants.iter() {
        if *constant_value == search_value {
            return Some(*name);
        }
    }
    
    None
}

pub const fn flag_description(bank: u8, bit: u8) -> Option<&'static str> {
    Some(match (bank, bit) {
        (1, 1) => "B scenario",
        _ => return None,
    })
}

#[derive(Debug, Clone)]
pub struct NameStore {
    names: HashMap<String, ArgValue>,
}

impl NameStore {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
        }
    }
    
    pub fn add(&mut self, name: String, value: ArgValue) {
        self.names.insert(name, value);
    }
    
    pub fn clear(&mut self) {
        self.names.clear();
    }
    
    pub fn get_value(&self, name: &str) -> Option<ArgValue> {
        self.names.get(name).or_else(|| SCRIPT_CONSTANTS.get(name)).copied()
    }
    
    pub fn get_name(&self, search: ArgValue) -> Option<String> {
        for (name, value) in &self.names {
            if *value == search {
                return Some(name.clone());
            }
        }
        
        search_constant(search).map(String::from)
    }
}