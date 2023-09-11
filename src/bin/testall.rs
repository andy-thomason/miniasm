use std::io::Write;


fn gen_sib_block<W : Write>(w: &mut W, op: &str, s: &str, sr: &str, scale: &str, sdisp: &str, gen_empty: bool) -> std::io::Result<()> {
    writeln!(w, " {op} {s}, {sdisp}(%rax,{sr},{scale})")?;
    writeln!(w, " {op} {s}, {sdisp}(%rcx,{sr},{scale})")?;
    writeln!(w, " {op} {s}, {sdisp}(%rdx,{sr},{scale})")?;
    writeln!(w, " {op} {s}, {sdisp}(%rbx,{sr},{scale})")?;
    writeln!(w, " {op} {s}, {sdisp}(%rsp,{sr},{scale})")?;
    if gen_empty { writeln!(w, " {op} {s}, {sdisp}(,{sr},{scale})")?; }
    writeln!(w, " {op} {s}, {sdisp}(%rsi,{sr},{scale})")?;
    writeln!(w, " {op} {s}, {sdisp}(%rdi,{sr},{scale})")?;
    Ok(())
}

fn gen_modrm<W : Write>(w: &mut W, op: &str, s: &str, disp: &str, ipbp: &str, gen_empty: bool) -> std::io::Result<()> {
    writeln!(w, " {op} {s}, {disp}(%rax)")?;
    writeln!(w, " {op} {s}, {disp}(%rcx)")?;
    writeln!(w, " {op} {s}, {disp}(%rdx)")?;
    writeln!(w, " {op} {s}, {disp}(%rbx)")?;

    gen_sib_block(w, op, s, "%rax", "1", disp, gen_empty)?;
    gen_sib_block(w, op, s, "%rcx", "1", disp, gen_empty)?;
    gen_sib_block(w, op, s, "%rdx", "1", disp, gen_empty)?;
    gen_sib_block(w, op, s, "%rbx", "1", disp, gen_empty)?;
    writeln!(w, " {op} {s}, {disp}(%rsp)")?;
    if gen_empty { writeln!(w, " {op} {s}, 0")? }
    gen_sib_block(w, op, s, "%rsi", "1", disp, gen_empty)?;
    gen_sib_block(w, op, s, "%rdi", "1", disp, gen_empty)?;

    gen_sib_block(w, op, s, "%rax", "2", disp, false)?;
    gen_sib_block(w, op, s, "%rcx", "2", disp, false)?;
    gen_sib_block(w, op, s, "%rdx", "2", disp, false)?;
    gen_sib_block(w, op, s, "%rbx", "2", disp, false)?;
    // writeln!(w, " {op} {s}, {disp}(%rsp)")?;
    gen_sib_block(w, op, s, "%rbp", "2", disp, false)?;
    gen_sib_block(w, op, s, "%rsi", "2", disp, false)?;
    gen_sib_block(w, op, s, "%rdi", "2", disp, false)?;

    gen_sib_block(w, op, s, "%rax", "4", disp, false)?;
    gen_sib_block(w, op, s, "%rcx", "4", disp, false)?;
    gen_sib_block(w, op, s, "%rdx", "4", disp, false)?;
    gen_sib_block(w, op, s, "%rbx", "4", disp, false)?;
    // writeln!(w, " {op} {s}, {disp}(%rsp)")?;
    gen_sib_block(w, op, s, "%rbp", "4", disp, false)?;
    gen_sib_block(w, op, s, "%rsi", "4", disp, false)?;
    gen_sib_block(w, op, s, "%rdi", "4", disp, false)?;

    gen_sib_block(w, op, s, "%rax", "8", disp, false)?;
    gen_sib_block(w, op, s, "%rcx", "8", disp, false)?;
    gen_sib_block(w, op, s, "%rdx", "8", disp, false)?;
    gen_sib_block(w, op, s, "%rbx", "8", disp, false)?;
    // writeln!(w, " {op} {s}, {disp}(%rsp)", disp, false)?;
    gen_sib_block(w, op, s, "%rbp", "8", disp, false)?;
    gen_sib_block(w, op, s, "%rsi", "8", disp, false)?;
    gen_sib_block(w, op, s, "%rdi", "8", disp, false)?;

    writeln!(w, " {op} {s}, {disp}(%{ipbp})")?;
    writeln!(w, " {op} {s}, {disp}(%rsi)")?;
    writeln!(w, " {op} {s}, {disp}(%rdi)")?;
    Ok(())
}

fn gen_ins<W : Write>(w: &mut W, op: &str, regs: &[&str; 8]) -> std::io::Result<()> {
    for s in regs {
        gen_modrm(w, op, s, "", "rip", true)?;
    }
    for s in regs {
        gen_modrm(w, op, s, "127", "rbp", false)?;
    }
    for s in regs {
        gen_modrm(w, op, s, "128", "rbp", false)?;
    }
    for s in regs {
        for d in regs {
            writeln!(w, " {op} {s}, {d}")?;
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = std::fs::File::create("testall.s")?;

    writeln!(file, " .text")?;

    let alah = ["%al", "%cl", "%dl", "%bl", "%ah", "%ch", "%dh", "%bh"];
    let w = &mut file;

    gen_ins(w, "addb", &alah)?;


    let mut file = std::fs::File::create("modrm2.s")?;

    writeln!(file, " .text")?;

    for mrm in 0_u32..0x100 {
        let (i1, i2, i3, i4) = match mrm >> 6 {
            0 => ("", ", 0x00, 0x00, 0x00, 0x00", "", ", 0x00, 0x00, 0x00, 0x00"),
            1 => (", 0x00", ", 0x00", ", 0x00", ", 0x00"),
            2 => (", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00", ", 0x00, 0x00, 0x00, 0x00"),
            _ => ("", "", "", ""),
        };
        if mrm % 8 == 5 {
            writeln!(file, "    .byte 0x00, 0x{mrm:02x}{i2}")?;
        } else if mrm % 8 != 4 || (mrm >> 6) == 3 {
            writeln!(file, "    .byte 0x00, 0x{mrm:02x}{i1}")?;
        } else  {
            for sib in 0_u32..0x100 {
                if sib % 8 == 5 {
                    writeln!(file, "    .byte 0x00, 0x{mrm:02x}, 0x{sib:02x}{i4}")?;
                } else {
                    writeln!(file, "    .byte 0x00, 0x{mrm:02x}, 0x{sib:02x}{i3}")?;
                }
            }
        }
    }

    Ok(())
}    
