use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use strum::IntoEnumIterator;
use strum_macros::{EnumCount, EnumIter};

use crate::backend::{Loc, ASM};
pub use Instruction::*;

// Order is important as it determines the register allocation order, so non-callee-save
// registers should be at the beginning.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumIter, EnumCount, PartialOrd, Ord)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    RSP,
    RBP,
    RBX,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    pub fn is_callee_save(&self) -> bool {
        match self {
            Register::RBP
            | Register::RBX
            | Register::R12
            | Register::R13
            | Register::R14
            | Register::R15 => true,
            _ => false,
        }
    }

    pub fn usable_registers() -> VecDeque<Register> {
        let mut q = Register::iter().rev().collect::<VecDeque<_>>();
        q.retain(|r| !vec![Register::RSP, Register::RBP].contains(r));
        q
    }

    pub fn to_1byte(&self) -> String {
        match self {
            Register::RAX => "%al".to_string(),
            Register::RBX => "%bl".to_string(),
            Register::RCX => "%cl".to_string(),
            Register::RDX => "%dl".to_string(),
            Register::R8 => "%r8b".to_string(),
            Register::R9 => "%r9b".to_string(),
            Register::R10 => "%r10b".to_string(),
            Register::R11 => "%r11b".to_string(),
            Register::R12 => "%r12b".to_string(),
            Register::R13 => "%r13b".to_string(),
            Register::R14 => "%r14b".to_string(),
            Register::R15 => "%r15b".to_string(),
            _ => panic!("Cannot convert to 1 byte register"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "%{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Reg(Register),
    Imm(i64),
    Mem(Register, i64, Option<Register>, Option<i64>), // base, offset, index, scale
}

impl Operand {
    pub fn to_loc(&self) -> Loc {
        match self {
            Operand::Reg(reg) => Loc::Reg(reg.clone()),
            Operand::Mem(r, offset, ..) => {
                assert_eq!(r, &Register::RBP);
                Loc::Stack(*offset / -8)
            }
            _ => panic!("Cannot convert to Loc"),
        }
    }

    pub fn get_inner_register(&self) -> Register {
        match self {
            Operand::Reg(reg) => reg.clone(),
            Operand::Mem(r, ..) => r.clone(),
            _ => panic!("Cannot get inner register"),
        }
    }

    #[allow(dead_code)]
    pub fn to_imm(&self) -> i64 {
        match self {
            Operand::Imm(v) => *v,
            _ => panic!("Cannot convert to i64"),
        }
    }

    pub fn to_register(&self) -> Register {
        match self {
            Operand::Reg(reg) => reg.clone(),
            _ => panic!("Cannot convert to Register"),
        }
    }

    pub fn is_reg(&self) -> bool {
        match self {
            Operand::Reg(_) => true,
            _ => false,
        }
    }

    pub fn is_mem(&self) -> bool {
        match self {
            Operand::Mem(..) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_imm(&self) -> bool {
        match self {
            Operand::Imm(_) => true,
            _ => false,
        }
    }
}

pub fn to_hex(v: i64, hide_zero: bool) -> String {
    if v == 0 && hide_zero {
        return "".to_string();
    }

    if v < 0 {
        format!("-0x{:x}", -(v as i128))
    } else {
        format!("0x{:x}", v)
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Imm(v) => write!(f, "${}", to_hex(*v, false)),
            Operand::Mem(r, offset, Some(index), Some(1))
            | Operand::Mem(r, offset, Some(index), None) => {
                write!(f, "{}({},{})", to_hex(*offset, true), r, index)
            }
            Operand::Mem(r, offset, Some(index), Some(scale)) => {
                write!(f, "{}({},{},{})", to_hex(*offset, true), r, index, scale)
            }
            Operand::Mem(r, offset, None, None) => write!(f, "{}({})", to_hex(*offset, true), r),
            Operand::Mem(..) => panic!("Invalid memory operand"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Instruction {
    MOV(Operand, Operand),
    MOVB(Operand, Operand),
    MOVZXB(Operand, Operand),
    MOVABS(Operand, Operand),
    LEA(Operand, Operand),
    SAL(Operand, Operand),
    SAR(Operand, Operand),
    INC(Operand),
    DEC(Operand),
    ADD(Operand, Operand),
    SUB(Operand, Operand),
    IMUL(Operand, Operand),
    CQO,
    DIV(Operand),
    CMP(Operand, Operand),
    TEST(Operand, Operand),
    JMP(String),
    JE(String),
    JNE(String),
    JG(String),
    JGE(String),
    JL(String),
    JLE(String),
    CALL(String),
    #[allow(non_camel_case_types)]
    CALL_DEREF(Operand),
    RET,
    PUSH(Operand),
    POP(Operand),
    LABEL(String),
    NEG(Operand),
    XOR(Operand, Operand),
    XCHG(Operand, Operand),
    LEAVE,
    CUSTOM(String),
    #[allow(non_camel_case_types)]
    ALIGN_PLACEHOLDER(i64), // value is number of arguments on the stack
}

impl Instruction {
    pub fn get_operands(&mut self) -> Vec<&mut Operand> {
        match self {
            MOV(op1, op2) => vec![op1, op2],
            MOVB(op1, op2) => vec![op1, op2],
            MOVABS(op1, op2) => vec![op1, op2],
            MOVZXB(op1, op2) => vec![op1, op2],
            LEA(op1, op2) => vec![op1, op2],
            SAL(op1, op2) => vec![op1, op2],
            SAR(op1, op2) => vec![op1, op2],
            INC(op) => vec![op],
            DEC(op) => vec![op],
            ADD(op1, op2) => vec![op1, op2],
            SUB(op1, op2) => vec![op1, op2],
            IMUL(op1, op2) => vec![op1, op2],
            DIV(op) => vec![op],
            CMP(op1, op2) => vec![op1, op2],
            TEST(op1, op2) => vec![op1, op2],
            PUSH(op) => vec![op],
            POP(op) => vec![op],
            NEG(op) => vec![op],
            XOR(op1, op2) => vec![op1, op2],
            XCHG(op1, op2) => vec![op1, op2],
            CALL_DEREF(op) => vec![op],
            LABEL(_) | JMP(_) | JE(_) | JNE(_) | JG(_) | JGE(_) | JL(_) | JLE(_) | CALL(_)
            | RET | CQO | LEAVE | CUSTOM(_) | ALIGN_PLACEHOLDER(_) => vec![],
        }
    }

    pub fn get_jmp_label(&self) -> Option<&String> {
        match self {
            JMP(label) | JE(label) | JNE(label) | JG(label) | JGE(label) | JL(label)
            | JLE(label) => Some(label),
            _ => None,
        }
    }

    pub fn is_cond_jump(&self) -> bool {
        match self {
            JE(_) | JNE(_) | JG(_) | JGE(_) | JL(_) | JLE(_) => true,
            _ => false,
        }
    }

    pub fn get_reversed_cond_jump(&self, new_label: &String) -> Instruction {
        match self {
            JE(_) => JNE(new_label.clone()),
            JNE(_) => JE(new_label.clone()),
            JG(_) => JLE(new_label.clone()),
            JGE(_) => JL(new_label.clone()),
            JL(_) => JGE(new_label.clone()),
            JLE(_) => JG(new_label.clone()),
            _ => panic!("Cannot get reversed conditional jump"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            MOV(dest, src) => write!(f, "\tmovq\t{}, {}", src, dest),
            MOVB(dest, src) => write!(f, "\tmovb\t{}, {}", src, dest),
            MOVABS(dest, src) => write!(f, "\tmovabsq\t{}, {}", src, dest),
            MOVZXB(dest, src) => write!(f, "\tmovzxb\t{}, {}", src, dest),
            LEA(dest, src) => write!(f, "\tleaq\t{}, {}", src, dest),
            SAL(op, shift) => write!(f, "\tsalq\t{}, {}", shift, op),
            SAR(op, shift) => write!(f, "\tsarq\t{}, {}", shift, op),
            ADD(dest, src) => write!(f, "\taddq\t{}, {}", src, dest),
            SUB(dest, src) => write!(f, "\tsubq\t{}, {}", src, dest),
            IMUL(dest, src) => write!(f, "\timulq\t{}, {}", src, dest),
            INC(op) => write!(f, "\tincq\t{}", op),
            DEC(op) => write!(f, "\tdecq\t{}", op),
            CQO => write!(f, "\tcqo"),
            DIV(src) => write!(f, "\tidivq\t{}", src),
            CMP(op1, op2) => write!(f, "\tcmpq\t{}, {}", op2, op1),
            TEST(op1, op2) => write!(f, "\ttestq\t{}, {}", op1, op2),
            JMP(label) => write!(f, "\tjmp\t{}", label),
            JE(label) => write!(f, "\tje\t{}", label),
            JNE(label) => write!(f, "\tjne\t{}", label),
            JG(label) => write!(f, "\tjg\t{}", label),
            JGE(label) => write!(f, "\tjge\t{}", label),
            JL(label) => write!(f, "\tjl\t{}", label),
            JLE(label) => write!(f, "\tjle\t{}", label),
            CALL(label) => write!(f, "\tcall\t{}", label),
            CALL_DEREF(op) => write!(f, "\tcall\t*{}", op),
            RET => write!(f, "\tret"),
            PUSH(op) => write!(f, "\tpushq\t{}", op),
            POP(op) => write!(f, "\tpopq\t{}", op),
            LABEL(label) => write!(f, "{}:", label),
            NEG(op) => write!(f, "\tnegq\t{}", op),
            XOR(op1, op2) => write!(f, "\txorq\t{}, {}", op2, op1),
            XCHG(op1, op2) => write!(f, "\txchgq\t{}, {}", op1, op2),
            LEAVE => write!(f, "\tleave"),
            CUSTOM(s) => write!(f, "{}", s),
            ALIGN_PLACEHOLDER(_) => unreachable!("ALIGN_PLACEHOLDER should not be printed"),
        }
    }
}

pub fn code_to_string(code: &ASM) -> String {
    code.iter()
        .map(|i| format!("{}", i))
        .collect::<Vec<String>>()
        .join("\n")
}
