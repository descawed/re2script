use std::fs;
use std::io::Write;
use std::path::Path;

use anyhow::{bail, Context, Result};
use encoding_rs::UTF_16LE;

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

fn maybe_write_funcs(dir: &Path, prefix: &str, funcs: Option<&[impl AsRef<[u8]>]>) -> Result<()> {
    let Some(funcs) = funcs else {
        return Ok(());
    };
    
    for (i, func) in funcs.iter().enumerate() {
        let path = dir.join(format!("{}{:02}.scd", prefix, i));
        fs::write(path, func.as_ref())?;   
    }
    
    Ok(())
}

fn read_ini(path: &Path) -> Result<String> {
    let raw_ini = fs::read(path)?;
    Ok(UTF_16LE.decode(&raw_ini).0.into())
}

fn write_ini(path: &Path, ini: &str) -> Result<()> {
    let raw_ini = ini.encode_utf16().flat_map(|c| c.to_le_bytes().into_iter()).collect::<Vec<_>>();
    let mut file = fs::File::create(path)?;
    file.write_all(&[0xff, 0xfe])?; // BOM
    file.write_all(&raw_ini)?;
    Ok(())
}

pub fn write_script<T: AsRef<[u8]>>(dir: &Path, main_funcs: Option<&[T]>, sub_funcs: Option<&[T]>) -> Result<()> {
    let script_dir = dir.join("Script");
    if !script_dir.is_dir() {
        bail!("Script directory not found");
    }

    // update ini file
    let mut found_ini = false;
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().unwrap_or_default().to_ascii_lowercase() == "ini" {
            let ini = read_ini(&path)?;
            let mut new_ini = String::new();
            let mut found_main = false;
            let mut found_sub = false;
            for line in ini.lines() {
                if let (true, Some(main_funcs)) = (line.starts_with("nScript"), main_funcs) {
                    new_ini.push_str(&format!("nScript		{}", main_funcs.len()));
                    found_main = true;
                } else if let (true, Some(sub_funcs)) = (line.starts_with("nScd_sub"), sub_funcs) {
                    new_ini.push_str(&format!("nScd_sub	{}", sub_funcs.len()));
                    found_sub = true;
                } else {
                    new_ini.push_str(line);
                }
                new_ini.push_str("\r\n");
            }
            
            if main_funcs.is_some() && !found_main {
                bail!("Could not find nScript in ini file");
            }
            
            if sub_funcs.is_some() && !found_sub {
                bail!("Could not find nScd_sub in ini file");
            }
            
            write_ini(&path, &new_ini)?;
            
            found_ini = true;
            
            break;
        }
    }
    
    if !found_ini {
        bail!("Could not find ini file");   
    }
    
    maybe_write_funcs(&script_dir, "main", main_funcs)?;
    maybe_write_funcs(&script_dir, "sub", sub_funcs)
}