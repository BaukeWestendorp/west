use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;

/// West runner
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to run
    #[arg(short, long)]
    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file_name = args.file.canonicalize().unwrap().to_string_lossy().to_string();

    let mut source_file = std::fs::File::open(args.file).expect("file should exist");
    let mut source = String::new();
    source_file.read_to_string(&mut source).expect("file should be readable");

    let session = parser::session::ParserSession::new(file_name, &source);
    let file = parser::Parser::new(session).parse();

    match file {
        Ok(file) => {
            println!("File has been run successfully");
            println!("{:?}", file);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }
}
