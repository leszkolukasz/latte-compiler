mod backend;
mod common;
mod compiler;
mod error;
mod frontend;
mod parser;

use crate::backend::asm::code_to_string;
use crate::backend::ASM;
use crate::error::{fail_with, report_error};
use crate::parser::{Program, Type};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::{env, fs};
use tempfile::NamedTempFile;
use crate::frontend::Stats;

#[derive(Debug, Clone, Default)]
pub struct NodeData {
    expr_typ: Option<Type>,
}

// TODO: For now this only works for expressions.
// If it is needed for something other than expr, deal with .clone()'s for routines and classes.
// Maybe this needs to be changed to &mut NodeData in Enriched?
pub type Ext = NodeData;

fn run_program(path: &str, args: Vec<String>) -> (String, String) {
    let result = Command::new(path)
        .args(args)
        .output()
        .expect("Failed to execute process");
    let stdout = String::from_utf8(result.stdout).expect("Failed to read stdout");
    let stderr = String::from_utf8(result.stderr).expect("Failed to read stderr");

    if !result.status.success() {
        fail_with!({
            eprintln!("{stdout}");
            eprintln!("{stderr}");
            eprintln!("Process {path} exited with non-zero exit code");
        })
    };

    (stdout, stderr)
}

fn read_and_append(src_path: &str, dest: &mut File) {
    let src = fs::read(src_path).expect("Failed to read file");
    dest.write_all(&src).expect("Failed to write to file");
}

fn run_transpiler(file_path: &str) -> String {
    let mut tmp_file = NamedTempFile::new().expect("Failed to create temporary file");

    read_and_append(file_path, tmp_file.as_file_mut());
    read_and_append("./lib/std.lat", tmp_file.as_file_mut());

    run_program(
        "./lib/transpiler",
        vec![tmp_file.path().to_str().unwrap().to_string()],
    )
    .0
}

fn run_parser(transpiled_source: &str) -> Program<Ext> {
    let parser_result = parser::parse::<Ext>(transpiled_source);

    if let Err(err) = parser_result {
        fail_with!({ report_error("Unknown".into(), transpiled_source, err) })
    }

    parser_result.unwrap()
}

fn run_frontend(
    filename: &str,
    original_source: &str,
    program: &mut Program<Ext>,
) -> Stats {
    match frontend::check(program) {
        Ok(stats) => {
            eprintln!("OK");
            stats
        }
        Err(err) => fail_with!({ report_error(filename.into(), original_source, err) }),
    }
}

fn run_backend(program: &Program<Ext>, stats: &Stats) -> ASM {
    backend::compile(program, stats).expect("Failed to compile program")
}

fn export_code(file_path: &Path, code: &ASM) -> String {
    let path = file_path.with_extension("s");

    let mut file = File::create(&path).unwrap();
    file.write_all(code_to_string(code).as_bytes()).unwrap();

    path.to_str().unwrap().to_string()
}

fn run_postprocessing(file_path: &str) {
    let path = Path::new(file_path);
    let args: Vec<String> = vec![
        "-o".into(),
        path.with_extension("").to_str().unwrap().into(),
        path.to_str().unwrap().into(),
        "./lib/runtime.c".into(),
    ];

    run_program("gcc", args);
}

fn main() {
    let file_path_str = env::args().nth(1).expect("Expected file argument");
    let file_path = Path::new(&file_path_str);
    let file_name = file_path.file_name().unwrap().to_str().unwrap();

    let original_source = fs::read_to_string(&file_path).expect("Source file does not exist");
    let transpiled_source = run_transpiler(&file_path_str);
    let mut program = run_parser(&transpiled_source);

    let stats = run_frontend(&file_name, &original_source, &mut program);
    let code = run_backend(&program, &stats);

    let exported_file = export_code(&file_path, &code);
    run_postprocessing(&exported_file);
}
