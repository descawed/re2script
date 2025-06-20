#[cfg(not(feature = "cli"))]
compile_error!("The CLI feature must be enabled to compile the binary");

#[cfg(feature = "cli")]
fn main() -> anyhow::Result<()> {
    re2script::cli::cli_main()
}