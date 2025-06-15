use anyhow::Result;
use residat::re2::{RawRdt, RdtSection};

mod instruction;
pub use instruction::*;
mod parse;
pub use parse::*;
mod constants;
pub use constants::*;

pub fn parse_scripts_from_rdt(rdt: &RawRdt) -> Result<(Function, Vec<Function>)> {
    let init_buf = rdt.section(RdtSection::InitScript);
    let exec_buf = rdt.section(RdtSection::ExecScript);
    
    let init_instructions = Instruction::read_function(init_buf);
    let exec_script = Instruction::read_script(exec_buf)?;
    
    let init_function = Function::from_instructions("init", &init_instructions);
    let exec_functions = Function::from_script(&exec_script);
    
    Ok((init_function, exec_functions))
}