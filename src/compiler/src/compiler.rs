use crate::parser::Program;

#[allow(dead_code)]
pub trait Compiler {
    type ExtendedData: Default;
    fn compile(&mut self, filename: &str, program: Program<Self::ExtendedData>) -> String;
}
