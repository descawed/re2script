use std::fs;
use std::path::Path;

use anyhow::{bail, Context, Result};

pub fn read_script(dir: &Path) -> Result<(Vec<Vec<u8>>, Vec<Vec<u8>>)> {
    let script_dir = dir.join("Script");
    if !script_dir.is_dir() {
        bail!("Script directory not found");
    }
    
    let mut main_files = Vec::new();
    let mut sub_files = Vec::new();
    
    for entry in fs::read_dir(script_dir)? {
        let entry = entry?;
        let path = entry.path();
        let Some(file_name) = path.file_name().map(|n| n.to_ascii_lowercase().to_string_lossy().to_string()) else {
            continue;
        };
        if path.is_file() && path.extension().unwrap_or_default().to_ascii_lowercase() == "scd" {
            if file_name.starts_with("main") {
                let index: i32 = file_name[4..6].parse().with_context(|| format!("Invalid main script file name: {}", file_name))?;
                main_files.push((index, fs::read(path)?));
            } else if file_name.starts_with("sub") {
                let index: i32 = file_name[3..5].parse().with_context(|| format!("Invalid sub script file name: {}", file_name))?;
                sub_files.push((index, fs::read(path)?));
            }
        }
    }
    
    main_files.sort_by_key(|(i, _)| *i);
    sub_files.sort_by_key(|(i, _)| *i);
    
    let main_funcs = main_files.into_iter().map(|(_, f)| f).collect();
    let sub_funcs = sub_files.into_iter().map(|(_, f)| f).collect();
    
    Ok((main_funcs, sub_funcs))
}