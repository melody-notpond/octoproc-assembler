use crate::{u9, u12};
use super::lexer::{Lexer, Token, TokenValue};

#[derive(Debug)]
pub enum Reg {
    Zero = 0,
    Ra = 1,
    Sp = 2,
    Fp = 3,
    A0 = 4,
    A1 = 5,
    T0 = 6,
    T1 = 7
}

#[derive(Debug)]
pub enum OpType {
    Add,
    Sub,
    OctitShiftRight
}

#[derive(Debug)]
pub enum BranchType {
    LessThanSigned,
    LessThanUnsigned,
    Equal,
    NotEqual
}

#[derive(Debug)]
pub enum LhiType {
    LoadLowerImmediate,
    AddUpperImmediate
}

#[derive(Debug)]
pub enum Instruction {
    Load(Reg, u9, Reg),
    Store(Reg, u9, Reg),
    OpImm(OpType, Reg, Reg, u9),
    OpReg(OpType, Reg, Reg, Reg),
    Jal(Reg, String),
    Jalr(Reg, Reg),
    Branch(BranchType, Reg, Reg, String),
    Lhi(LhiType, Reg, u12)
}

#[derive(Debug)]
pub struct Line {
    pub lino: usize,
    pub label: Option<String>,
    pub instr: Option<Instruction>,
}

#[derive(Debug)]
pub enum ErrorType {
    InvalidOp,
    InvalidReg,
    InvalidOperand,
    UnexpectedSymbol,
    UnexpectedEof
}

#[derive(Debug)]
pub struct Error {
    pub lino: usize,
    pub type_: ErrorType,
}

fn parse_register(lexer: &mut Lexer) -> Result<Reg, Error> {
    let state = lexer.save();
    if let Some(Token { value: TokenValue::Symbol(sym), .. }) = lexer.next() {
        match sym.as_str() {
            "x0" | "zero" => Ok(Reg::Zero),
            "x1" | "ra" => Ok(Reg::Ra),
            "x2" | "sp" => Ok(Reg::Sp),
            "x3" | "fp" => Ok(Reg::Fp),
            "x4" | "a0" => Ok(Reg::A0),
            "x5" | "a1" => Ok(Reg::A1),
            "x6" | "t0" => Ok(Reg::T0),
            "x7" | "t1" => Ok(Reg::T1),

            _ => {
                lexer.recall(state);
                Err(Error {
                    lino: lexer.get_lino(),
                    type_: ErrorType::InvalidReg,
                })
            }
        }
    } else {
        lexer.recall(state);
        Err(Error {
            lino: lexer.get_lino(),
            type_: ErrorType::InvalidReg,
        })
    }
}

macro_rules! expect {
    ($lexer: ident, $($pats: pat),+) => {
        match $lexer.next() {
            Some($(Token { value: $pats, .. })|+) => (),
            Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::UnexpectedSymbol }),
            _ => return Err(Error { lino: $lexer.get_lino(), type_: ErrorType::UnexpectedEof })
        }
    };
}

pub fn parse(filename: &str, contents: &str) -> Result<Vec<Line>, Error> {
    let mut lexer = Lexer::new(filename, contents);
    let mut result = vec![];

    enum Mode {
        MemoryAccess,
        OpImm,
        OpReg,
        Jal,
        Jalr,
        Branch,
        Lhi,
    }

    while !lexer.eof() {
        while let Some(Token { value: TokenValue::Newline, .. }) = lexer.peek() {
            lexer.next();
        }

        let (mut op, lino) = match lexer.next() {
            Some(Token { value: TokenValue::Symbol(sym), lino, .. }) => (sym, lino),
            Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOp }),
            None => break,
        };

        let mut line = Line {
            lino,
            label: None,
            instr: None,
        };

        if let Some(Token { value: TokenValue::Colon, .. }) = lexer.peek() {
            line.label = Some(op);
            lexer.next();
            match lexer.next() {
                Some(Token { value: TokenValue::Symbol(sym), .. }) => op = sym,
                Some(Token { value: TokenValue::Newline, .. }) => {
                    result.push(line);
                    continue;
                }
                Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOp }),
                None => break,
            };
        }

        let mode = match op.as_str() {
            "load" | "store" => Mode::MemoryAccess,
            "addi" | "subi" | "osri" => Mode::OpImm,
            "add" | "sub" => Mode::OpReg,
            "jal" => Mode::Jal,
            "jalr" => Mode::Jalr,
            "blt" | "bltu" | "beq" | "bne" => Mode::Branch,
            "lli" | "aui" => Mode::Lhi,

            _ => return Err(Error { lino, type_: ErrorType::InvalidOp }),
        };

        let instr = match mode {
            Mode::MemoryAccess => {
                // OP rd, imm(r0)
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let imm = match lexer.next() {
                    Some(Token { value: TokenValue::U9(i), .. }) => i,
                    Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOperand }),
                    None => return Err(Error { lino: lexer.get_lino(), type_: ErrorType::UnexpectedEof }),
                };

                expect!(lexer, TokenValue::LParen);
                let r0 = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::RParen);

                match op.as_str() {
                    "load" => Instruction::Load(rd, imm, r0),
                    "store" => Instruction::Store(rd, imm, r0),
                    _ => unreachable!(),
                }
            }

            Mode::OpImm => {
                // OP rd, r0, imm
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let r0 = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let imm = match lexer.next() {
                    Some(Token { value: TokenValue::U9(i), .. }) => i,
                    Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOperand }),
                    None => return Err(Error { lino: lexer.get_lino(), type_: ErrorType::UnexpectedEof }),
                };

                match op.as_str() {
                    "addi" => Instruction::OpImm(OpType::Add, rd, r0, imm),
                    "subi" => Instruction::OpImm(OpType::Sub, rd, r0, imm),
                    "osri" => Instruction::OpImm(OpType::OctitShiftRight, rd, r0, imm),
                    _ => unreachable!(),
                }
            }

            Mode::OpReg => {
                // OP rd, r0, r1
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let r0 = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let r1 = parse_register(&mut lexer)?;

                match op.as_str() {
                    "add" => Instruction::OpReg(OpType::Add, rd, r0, r1),
                    "sub" => Instruction::OpReg(OpType::Sub, rd, r0, r1),
                    _ => unreachable!(),
                }
            }

            Mode::Jal => {
                // jal rd, label
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let label = match lexer.next() {
                    Some(Token { value: TokenValue::Symbol(sym), .. }) => sym,
                    Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOperand }),
                    None => return Err(Error { lino: lexer.get_lino(), type_: ErrorType::UnexpectedEof }),
                };

                Instruction::Jal(rd, label)
            }

            Mode::Jalr => {
                // jalr rd, label
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let r0 = parse_register(&mut lexer)?;

                Instruction::Jalr(rd, r0)
            }

            Mode::Branch => {
                // OP rd, label
                let r0 = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let r1 = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let label = match lexer.next() {
                    Some(Token { value: TokenValue::Symbol(sym), .. }) => sym,
                    Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOperand }),
                    None => return Err(Error { lino: lexer.get_lino(), type_: ErrorType::UnexpectedEof }),
                };

                match op.as_str() {
                    "blt" => Instruction::Branch(BranchType::LessThanSigned, r0, r1, label),
                    "bltu" => Instruction::Branch(BranchType::LessThanUnsigned, r0, r1, label),
                    "beq" => Instruction::Branch(BranchType::Equal, r0, r1, label),
                    "bne" => Instruction::Branch(BranchType::NotEqual, r0, r1, label),
                    _ => unreachable!(),
                }
            }

            Mode::Lhi => {
                // OP rd, imm
                let rd = parse_register(&mut lexer)?;
                expect!(lexer, TokenValue::Comma);

                let imm = match lexer.next() {
                    Some(Token { value: TokenValue::U12(i) | TokenValue::U9(i), .. }) => i,
                    Some(Token { lino, .. }) => return Err(Error { lino, type_: ErrorType::InvalidOperand }),
                    None => return Err(Error { lino: lexer.get_lino(), type_: ErrorType::UnexpectedEof }),
                };

                match op.as_str() {
                    "lli" => Instruction::Lhi(LhiType::LoadLowerImmediate, rd, imm),
                    "aui" => Instruction::Lhi(LhiType::AddUpperImmediate, rd, imm),
                    _ => unreachable!(),
                }
            }
        };

        line.instr = Some(instr);
        result.push(line);
    }

    Ok(result)
}
