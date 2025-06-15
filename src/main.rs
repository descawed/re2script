use std::env;
use std::fs::File;
use std::io::BufReader;

use anyhow::{bail, Result};
use residat::re2::RawRdt;
use re2script::parse_scripts_from_rdt;

fn main() -> Result<()> {
    let Some(path) = env::args().nth(1) else {
        bail!("No RDT path provided");
    };
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let rdt = RawRdt::read(reader)?;
    let (init_script, exec_script) = parse_scripts_from_rdt(&rdt)?;

    println!("{}", init_script);
    std::thread::sleep(std::time::Duration::from_millis(500));
    for function in exec_script {
        println!("{}", function);
        std::thread::sleep(std::time::Duration::from_millis(500));
    }

    Ok(())
}