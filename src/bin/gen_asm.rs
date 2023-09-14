use std::io::Write;


fn gen_sib_block<W : Write>(w: &mut W, op: &str, s: &str, sr: &str, aregs: &[&str], scale: &str, sdisp: &str, gen_empty: bool, rev: bool) -> std::io::Result<()> {
    for i in 0..8 {
        let areg = aregs[i];
        if i != 5 {
            if !rev {
                writeln!(w, " {op} {s}, {sdisp}({areg},{sr},{scale})")?;
            } else {
                writeln!(w, " {op} {sdisp}({areg},{sr},{scale}), {s}")?;
            }
        } else {
            if gen_empty {
                if !rev {
                    writeln!(w, " {op} {s}, {sdisp}(,{sr},{scale})")?;
                } else {
                    writeln!(w, " {op} {sdisp}(,{sr},{scale}), {s}")?;
                }
            }
        }
    }
    Ok(())
}

fn gen_modrm<W : Write>(w: &mut W, op: &str, s: &str, disp: &str, ipbp: &str, aregs: &[&str], sregs: &[&str], gen_empty: bool, rev: bool) -> std::io::Result<()> {
    for i in 0..8 {
        let areg = aregs[i];
        if i < 4 || i > 5 {
            if !rev {
                writeln!(w, " {op} {s}, {disp}({areg})")?;
            } else {
                writeln!(w, " {op} {disp}({areg}), {s}")?;
            }
        } else if i == 5 {
            if !rev {
                writeln!(w, " {op} {s}, {disp}({ipbp})")?;
            } else {
                writeln!(w, " {op} {disp}({ipbp}), {s}")?;
            }
        } else if i == 4 {
            for sh in 0..4 {
                for j in 0..8 {
                    let sr = sregs[j];
                    if j != 4 && j != 5 {
                        let shift = ["1", "2", "4", "8"][sh];
                        gen_sib_block(w, op, s, sr, aregs, shift, disp, gen_empty, rev)?;
                    } else if j == 4 && sh == 0 {
                        if !rev {
                            writeln!(w, " {op} {s}, {disp}(%rsp)")?;
                        } else {
                            writeln!(w, " {op} {disp}(%rsp), {s}")?;
                        }
                    } else if j == 5 && sh == 0 {
                        if gen_empty {
                            if !rev {
                                writeln!(w, " {op} {s}, 1")?
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn gen_modrm_ins<W : Write>(w: &mut W, op: &str, regs: &[&str], aregs: &[&str], sregs: &[&str], rev: bool) -> std::io::Result<()> {
    for s in regs {
        gen_modrm(w, op, s, "", "%rip", aregs, sregs, true, rev)?;
    }
    for s in regs {
        gen_modrm(w, op, s, "127", "%rbp", aregs, sregs, false, rev)?;
    }
    for s in regs {
        gen_modrm(w, op, s, "128", "%rbp", aregs, sregs, false, rev)?;
    }
    if !rev {
        for s in regs {
            for d in regs {
                writeln!(w, " {op} {s}, {d}")?;
            }
        }
    }
    Ok(())
}

fn gen_imm_ins<W : Write>(w: &mut W, op: &str, regs: &[&str]) -> std::io::Result<()> {
    for r in regs {
        writeln!(w, " {op} $1, {r}")?;
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = std::fs::File::create("/tmp/testall.s")?;

    writeln!(file, " .text")?;

    const IREGS : [[&str; 16]; 5] = [
        // 8 Bit.
        ["%al", "%cl", "%dl", "%bl",
        "%spl", "%bpl", "%sil", "%dil",
        "%r8b", "%r9b", "%r10b", "%r11b",
        "%r12b", "%r13b", "%r14b", "%r15b"],
    
        // 16 Bit.
        ["%ax", "%cx", "%dx", "%bx",
        "%sp", "%bp", "%si", "%di",
        "%r8w", "%r9w", "%r10w", "%r11w",
        "%r12w", "%r13w", "%r14w", "%r15w"],

        // 32 Bit.
        ["%eax", "%ecx", "%edx", "%ebx",
        "%esp", "%ebp", "%esi", "%edi",
        "%r8d", "%r9d", "%r10d", "%r11d",
        "%r12d", "%r13d", "%r14d", "%r15d"],

        // 64 Bit.
        ["%rax", "%rcx", "%rdx", "%rbx",
        "%rsp", "%rbp", "%rsi", "%rdi",
        "%r8", "%r9", "%r10", "%r11",
        "%r12", "%r13", "%r14", "%r15"],

        // 8 Bit legacy
        ["%al", "%cl", "%dl", "%bl",
        "%ah", "%ch", "%dh", "%bh",
        "%r8b", "%r9b", "%r10b", "%r11b",
        "%r12b", "%r13b", "%r14b", "%r15b"],
    ];


    let w = &mut file;

    gen_modrm_ins(w, "addb", &IREGS[4][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
    gen_modrm_ins(w, "addl", &IREGS[2][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
    gen_modrm_ins(w, "addb", &IREGS[4][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
    gen_modrm_ins(w, "addl", &IREGS[2][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
    gen_imm_ins(w, "addb", &IREGS[4][0..8])?;
    gen_imm_ins(w, "addl", &IREGS[2][0..8])?;


    // let mut file = std::fs::File::create("modrm2.s")?;

    // writeln!(file, " .text")?;

    // for mrm in 0_u32..0x100 {
    //     let (i1, i2, i3, i4) = match mrm >> 6 {
    //         0 => ("", ", 0x00, 0x00, 0x00, 0x00", "", ", 0x00, 0x00, 0x00, 0x00"),
    //         1 => (", 0x00", ", 0x00", ", 0x00", ", 0x00"),
    //         2 => (", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00"),
    //         _ => ("", "", "", ""),
    //     };
    //     if mrm % 8 == 5 {
    //         writeln!(file, "    .byte 0x00, 0x{mrm:02x}{i2}")?;
    //     } else if mrm % 8 != 4 || (mrm >> 6) == 3 {
    //         writeln!(file, "    .byte 0x00, 0x{mrm:02x}{i1}")?;
    //     } else  {
    //         for sib in 0_u32..0x100 {
    //             if sib % 8 == 5 {
    //                 writeln!(file, "    .byte 0x00, 0x{mrm:02x}, 0x{sib:02x}{i4}")?;
    //             } else {
    //                 writeln!(file, "    .byte 0x00, 0x{mrm:02x}, 0x{sib:02x}{i3}")?;
    //             }
    //         }
    //     }
    // }

    Ok(())
}    
