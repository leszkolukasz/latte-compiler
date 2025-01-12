use crate::backend::{asm, ASM};
use crate::backend::asm::{Operand, Register};
use crate::backend::utils::{highest_power_of_two_divisor};

pub fn run(asm: ASM) -> ASM {
    optimize(
        asm,
        &[
            remove_unused_labels,
            remove_jmp_label_pattern,
            remove_jmp_jmp_label_pattern,
            optimize_expressions
        ],
        10,
    )
}

fn optimize(mut asm: ASM, passes: &[fn(ASM) -> ASM], max_iterations: usize) -> ASM {
    let mut iterations = 0;
    loop {
        let mut changed = false;
        for pass in passes {
            let optimized = pass(asm.clone());
            if optimized != asm {
                asm = optimized;
                changed = true;
            }
        }

        iterations += 1;
        if !changed || iterations >= max_iterations {
            break;
        }
    }
    asm
}

// If there is a jump to label that is
// the next instruction, remove jump.
fn remove_jmp_label_pattern(asm: ASM) -> ASM {
    let mut new_asm = ASM::new();

    let mut last_label: Option<String> = None;
    for instr in asm.into_iter().rev() {
        if let asm::LABEL(l) = &instr {
            last_label = Some(l.clone());
            new_asm.push_front(instr);
            continue;
        } else if let asm::JMP(label) = &instr {
            if let Some(ref prev_label) = last_label {
                if prev_label == label {
                    continue;
                }
            }
        }

        new_asm.push_front(instr);
        last_label = None;
    }

    new_asm
}

// If there is conditional jump <Label 1>; jump <Label 2>; <Label 1>; replace it with
// reversed conditional jump <Label 2>; <Label 1>;
fn remove_jmp_jmp_label_pattern(asm: ASM) -> ASM {
    let mut new_asm = ASM::new();

    let asm_vec = asm.into_iter().collect::<Vec<_>>();
    let mut i = 0;

    while i < asm_vec.len() {
        let instr = &asm_vec[i];

        if i + 2 < asm_vec.len() && instr.is_cond_jump() {
            let label_1 = instr.get_jmp_label().unwrap();
            let next_instr = &asm_vec[i + 1];

            if let asm::JMP(_) = next_instr {
                let next_next_instr = &asm_vec[i + 2];

                if let asm::LABEL(l) = next_next_instr {
                    if l == label_1 {
                        let label_2 = next_instr.get_jmp_label().unwrap();
                        let reversed_cond_jump = instr.get_reversed_cond_jump(label_2);
                        new_asm.push_back(reversed_cond_jump);
                        new_asm.push_back(next_next_instr.clone());

                        i += 3;
                        continue;
                    }
                }
            }
        }

        new_asm.push_back(instr.clone());
        i += 1;
    }

    new_asm
}

fn remove_unused_labels(asm: ASM) -> ASM {
    let mut new_asm = ASM::new();

    let mut used_labels = std::collections::HashSet::new();
    for instr in asm.iter() {
        if let Some(label) = instr.get_jmp_label() {
            used_labels.insert(label.clone());
        }
    }

    for instr in asm {
        if let asm::LABEL(label) = &instr {
            if !used_labels.contains(label) && label.starts_with(".") && !label.starts_with(".LS") {
                continue;
            }
        }

        new_asm.push_back(instr);
    }

    new_asm
}

fn get_lea_substitution(reg: &Register, mul: i64) -> Option<asm::Instruction> {
    match mul {
        3 | 5 | 9 => Some(
            asm::LEA(
                Operand::Reg(reg.clone()),
                Operand::Mem(
                    reg.clone(),
                    0,
                    Some(reg.clone()),
                    Some(mul - 1),
                ),
            ),
        ),
        _ => None,
    }
}

// Assumes `op` is a register or in memory.
fn get_mul_optimization(op: &Operand, mul: i64) -> Option<ASM> {
    assert!(!op.is_imm());

    let mut asm = ASM::new();

    if mul < 0 {
        assert_ne!(mul, i64::MIN);

        if let Some(new_asm) = get_mul_optimization(op, -mul) {
            asm.push_back(asm::NEG(op.clone()));
            asm.append(&mut new_asm.clone());
            return Some(asm);
        }

        return None;
    }

    if mul == 0 {
        asm.push_back(asm::XOR(op.clone(), op.clone()));
        return Some(asm);
    }

    if mul == 1 {
        return Some(asm);
    }

    let highest_power_of_two = highest_power_of_two_divisor(mul);

    if highest_power_of_two == mul {
        let shift = mul.trailing_zeros();
        asm.push_back(asm::SAL(op.clone(), Operand::Imm(shift as i64)));
        return Some(asm);
    }

    if op.is_mem() {
        return None
    }

    if let Some(asm_instr) = get_lea_substitution(&op.to_register(), mul / highest_power_of_two) {
        asm.push_back(asm_instr);

        let shift = highest_power_of_two.trailing_zeros();
        asm.push_back(asm::SAL(op.clone(), Operand::Imm(shift as i64)));

        return Some(asm);
    }

    None
}

fn optimize_expressions(asm: ASM) -> ASM {
    let mut new_asm = ASM::new();

    for instr in asm {
        match instr {
            asm::IMUL(ref op, Operand::Imm(mul)) => {
                if let Some(new_asm2) = get_mul_optimization(&op, mul) {
                    new_asm.append(&mut new_asm2.clone());
                } else {
                    new_asm.push_back(instr);
                }
            }
            _ => new_asm.push_back(instr),
        }
    }

    new_asm
}