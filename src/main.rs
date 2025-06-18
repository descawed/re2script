use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyhow::{bail, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};
use re2script::{Compiler, ScriptFormatter};
use residat::re2::{RawRdt, RdtSection};

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
enum Format {
    /// An RDT file
    Rdt,
    /// A previously extracted initialization script SCD
    Init,
    /// A previously extracted execution script SCD
    Exec,
}

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Args, Debug)]
#[group(required = true, multiple = true)]
struct ExtractPaths {
    /// Path to extract the init script to
    #[arg(short, long)]
    init_script: Option<PathBuf>,
    /// Path to extract the exec script to
    #[arg(short, long)]
    exec_script: Option<PathBuf>,
}

#[derive(Args, Debug)]
#[group(required = true, multiple = true)]
struct DecompilePaths {
    /// Path to write the decompiled script to
    output: Option<PathBuf>,
    /// Path to write the decompiled initialization script to
    ///
    /// Only relevant when decompiling from an RDT. You can specify just this option to decompile
    /// only the initialization script, or use both --init-output and --exec-output to decompile
    /// both scripts but save them to different locations.
    #[arg(short, long, conflicts_with = "output")]
    init_output: Option<PathBuf>,
    /// Path to write the decompiled execution script to
    ///
    /// Only relevant when decompiling from an RDT. You can specify just this option to decompile
    /// only the execution script, or use both --init-output and --exec-output to decompile
    /// both scripts but save them to different locations.
    #[arg(short, long, conflicts_with = "output")]
    exec_output: Option<PathBuf>,
}

#[derive(Args, Debug)]
#[group(required = true, multiple = true)]
struct CompilePaths {
    /// Path to write the compiled script to. This must either be an RDT or the script you compiled
    /// must have contained only one of either an init or exec script.
    output: Option<PathBuf>,
    /// Path to write the compiled initialization script to
    #[arg(short, long, conflicts_with = "output")]
    init_output: Option<PathBuf>,
    /// Path to write the compiled execution script to
    #[arg(short, long, conflicts_with = "output")]
    exec_output: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
enum Command {
    Extract {
        /// Path to the RDT file
        rdt: PathBuf,
        #[command(flatten)]
        output_paths: ExtractPaths,
    },
    Decompile {
        /// Path to the file to decompile
        input: PathBuf,
        #[command(flatten)]
        output_paths: DecompilePaths,
        /// The format of the input file
        #[arg(short, long, value_enum, default_value_t = Format::Rdt)]
        format: Format,
        /// Include a descriptive comment next to known ID values with no corresponding constant
        #[arg(short, long)]
        comment_ids: bool,
        /// Show all arguments for all instructions, including those which use the default value
        /// and those which are calculated automatically
        #[arg(short, long)]
        all_args: bool,
        /// How many displayed arguments an instruction can have before the formatter defaults to
        /// showing argument names (keywords) for all arguments
        #[arg(short, long, default_value_t = 2)]
        keyword_threshold: usize,
        /// Remove nop instructions from the decompiled scripts
        #[arg(short, long)]
        nop_suppress: bool,
    },
    Compile {
        /// Path to the file to compile
        input: PathBuf,
        #[command(flatten)]
        output_paths: CompilePaths,
        /// Indicates that the output file(s) should be an RDT that will be updated in place.
        /// Without this, the output file is assumed to be an SCD and simply overwritten.
        #[arg(short, long)]
        rdt: bool,
        /// When compiling to RDT, if a script is smaller than the one already in the RDT, pad it
        /// with zeros instead of shifting the other sections
        #[arg(short, long, requires = "rdt")]
        pad: bool,
    },
}

fn read_rdt(path: &Path) -> Result<RawRdt> {
    let file = File::open(path)?;
    Ok(RawRdt::read(file)?)
}

fn extract_scd(rdt_path: &Path, init_path: Option<&Path>, exec_path: Option<&Path>) -> Result<()> {
    let rdt = read_rdt(rdt_path)?;
    
    if let Some(init_path) = init_path {
        fs::write(init_path, rdt.section(RdtSection::InitScript))?;
    }
    
    if let Some(exec_path) = exec_path {
        fs::write(exec_path, rdt.section(RdtSection::ExecScript))?;
    }
    
    Ok(())
}

fn get_script_buffers(path: &Path, format: Format) -> Result<(Option<Vec<u8>>, Option<Vec<u8>>)> {
    Ok(match format {
        Format::Rdt => {
            let rdt = read_rdt(path)?;
            let init_buf = rdt.section(RdtSection::InitScript).to_vec();
            let exec_buf = rdt.section(RdtSection::ExecScript).to_vec();
            (Some(init_buf), Some(exec_buf))
        }
        Format::Init => {
            let buf = fs::read(path)?;
            (Some(buf), None)
        }
        Format::Exec => {
            let buf = fs::read(path)?;
            (None, Some(buf))
        }
    })
}

fn decompile(
    init_buf: Option<Vec<u8>>, init_path: Option<&Path>,
    exec_buf: Option<Vec<u8>>, exec_path: Option<&Path>,
    mut formatter: ScriptFormatter, combined_path: Option<&Path>,
) -> Result<()> {
    if init_buf.is_none() && init_path.is_some() {
        bail!("Cannot export an initialization script from an execution script");
    }
    
    if exec_buf.is_none() && exec_path.is_some() {
        bail!("Cannot export an execution script from an initialization script");
    }
    
    let init_path = init_path.or(combined_path);
    let parsed_init = if let (Some(init_buf), Some(_)) = (init_buf, init_path) {
        Some(formatter.parse_init_script(&init_buf))
    } else {
        None
    };
    
    let exec_path = exec_path.or(combined_path);
    let parsed_exec = if let (Some(exec_buf), Some(_)) = (exec_buf, exec_path) {
        Some(formatter.parse_exec_script(&exec_buf)?)
    } else {
        None
    };
    
    if let Some(combined_path) = combined_path {
        let mut out = String::new();
        if let Some(init) = parsed_init {
            out.push_str(&init.to_string());
            out.push_str("\n\n");
        }
        
        if let Some(exec) = parsed_exec {
            for func in exec {
                out.push_str(&func.to_string());
                out.push_str("\n\n");
            }
        }
        
        fs::write(combined_path, out)?;
    } else {
        if let (Some(init), Some(init_path)) = (parsed_init, init_path) {
            fs::write(init_path, init.to_string())?;
        }
        
        if let (Some(exec), Some(exec_path)) = (parsed_exec, exec_path) {
            let mut f = File::create(exec_path)?;
            for func in exec {
                f.write_all(&func.to_string().into_bytes())?;
                f.write_all(b"\n\n")?;
            }
        }
    }
    
    Ok(())
}

fn patch_rdt(rdt: &mut RawRdt, buf: Option<Vec<u8>>, section: RdtSection, pad: bool) {
    let Some(mut buf) = buf else {
        return;
    };

    if pad {
        let current_size = rdt.section(section).len();
        if buf.len() < current_size {
            buf.resize(current_size, 0);
        }
    }

    rdt.replace_section(section, buf);
}

fn update_rdt(path: &Path, buf: Vec<u8>, section: RdtSection, pad: bool) -> Result<()> {
    let mut rdt = read_rdt(path)?;
    patch_rdt(&mut rdt, Some(buf), section, pad);
    rdt.write(File::create(path)?)?;
    Ok(())
}

fn compile(input: &Path, output_paths: &CompilePaths, is_rdt: bool, pad: bool) -> Result<()> {
    let code = fs::read_to_string(input)?;
    
    let mut compiler = Compiler::new();
    compiler.parse(code)?;
    
    let (init, exec) = compiler.compile()?;
    
    if init.is_none() && output_paths.init_output.is_some() {
        bail!("Cannot compile an execution script to an initialization script");
    }
    
    if exec.is_none() && output_paths.exec_output.is_some() {
        bail!("Cannot compile an initialization script to an execution script");
    }
    
    if let Some(output_path) = output_paths.output.as_deref() {
        if is_rdt {
            let mut rdt = read_rdt(output_path)?;

            patch_rdt(&mut rdt, init, RdtSection::InitScript, pad);
            patch_rdt(&mut rdt, exec, RdtSection::ExecScript, pad);

            rdt.write(File::create(output_path)?)?;
        } else {
            match (init, exec) {
                (Some(_), Some(_)) => bail!("Cannot specify a single output file for both init and exec SCDs"),
                (Some(init_buf), None) => fs::write(output_path, init_buf)?,
                (None, Some(exec_buf)) => fs::write(output_path, exec_buf)?,
                _ => (),
            }
        }
    } else {
        if let (Some(init_path), Some(init_buf)) = (output_paths.init_output.as_deref(), init) {
            if is_rdt {
                update_rdt(init_path, init_buf, RdtSection::InitScript, pad)?;
            } else {
                fs::write(init_path, init_buf)?;
            }
        }
        
        if let (Some(exec_path), Some(exec_buf)) = (output_paths.exec_output.as_deref(), exec) {
            if is_rdt {
                update_rdt(exec_path, exec_buf, RdtSection::ExecScript, pad)?;
            } else {
                fs::write(exec_path, exec_buf)?;
            }
        }
    }
    
    Ok(())
}

fn main() -> Result<()> {
    let command = Cli::parse().command;
    
    match command {
        Command::Extract { rdt, output_paths } => {
            extract_scd(&rdt, output_paths.init_script.as_deref(), output_paths.exec_script.as_deref())
        }
        Command::Decompile { input, output_paths, format, comment_ids, all_args, keyword_threshold, nop_suppress } => {
            let (init, exec) = get_script_buffers(&input, format)?;
            let formatter = ScriptFormatter::new(comment_ids, all_args, keyword_threshold, nop_suppress);
            decompile(init, output_paths.init_output.as_deref(), exec, output_paths.exec_output.as_deref(), formatter, output_paths.output.as_deref())
        }
        Command::Compile { input, output_paths, rdt, pad } => {
            compile(&input, &output_paths, rdt, pad)
        }
    }
}