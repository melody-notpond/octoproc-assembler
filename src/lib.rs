pub mod parser;
pub mod lexer;

pub use octal_types::*;

#[allow(non_camel_case_types)]
pub mod octal_types {
    pub type u9 = u16;
    pub type u12 = u16;
    pub type u24 = u32;
}
