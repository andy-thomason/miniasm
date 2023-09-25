use std::{io::Write, process::{Command, Stdio}};

use log::info;
use miniasm::{encode_result::EncodeResult, x86::Instruction};
use simple_logger::SimpleLogger;

type BDEResult<T> = Result<T, Box<dyn std::error::Error>>;

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

const OPS : &[&str] = &["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"];

#[allow(dead_code)]
fn gen_sib_block1<W : Write>(w: &mut W, op: &str, sr: &str, aregs: &[&str], scale: &str, sdisp: &str, gen_empty: bool) -> std::io::Result<()> {
    for i in 0..8 {
        let areg = aregs[i];
        if i != 5 {
                writeln!(w, " {op} {sdisp}({areg},{sr},{scale})")?;
        } else {
            if gen_empty {
                writeln!(w, " {op} {sdisp}(,{sr},{scale})")?;
            }
        }
    }
    Ok(())
}

fn gen_sib_block2<W : Write>(w: &mut W, op: &str, s: &str, sr: &str, aregs: &[&str], scale: &str, sdisp: &str, gen_empty: bool, rev: bool) -> std::io::Result<()> {
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

#[allow(dead_code)]
fn gen_modrm1<W : Write>(w: &mut W, op: &str, disp: &str, ipbp: &str, aregs: &[&str], sregs: &[&str], gen_empty: bool) -> std::io::Result<()> {
    for i in 0..8 {
        let areg = aregs[i];
        if i < 4 || i > 5 {
            writeln!(w, " {op} {disp}({areg})")?;
        } else if i == 5 {
            writeln!(w, " {op} {disp}({ipbp})")?;
        } else if i == 4 {
            for sh in 0..4 {
                for j in 0..8 {
                    let sr = sregs[j];
                    if j != 4 && j != 5 {
                        let shift = ["1", "2", "4", "8"][sh];
                        gen_sib_block1(w, op, sr, aregs, shift, disp, gen_empty)?;
                    } else if j == 4 && sh == 0 {
                            writeln!(w, " {op} {disp}(%rsp)")?;
                    } else if j == 5 && sh == 0 {
                        if gen_empty {
                            writeln!(w, " {op} 1")?
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn gen_modrm2<W : Write>(w: &mut W, op: &str, s: &str, disp: &str, ipbp: &str, aregs: &[&str], sregs: &[&str], gen_empty: bool, rev: bool) -> std::io::Result<()> {
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
                        gen_sib_block2(w, op, s, sr, aregs, shift, disp, gen_empty, rev)?;
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

#[allow(dead_code)]
fn gen_modrm_ins1<W : Write>(w: &mut W, op: &str, rmregs: &[&str], aregs: &[&str], sregs: &[&str]) -> std::io::Result<()> {
    gen_modrm1(w, op, "", "%rip", aregs, sregs, true)?;
    gen_modrm1(w, op, "127", "%rbp", aregs, sregs, false)?;
    gen_modrm1(w, op, "128", "%rbp", aregs, sregs, false)?;
    for d in rmregs {
        writeln!(w, " {op} {d}")?;
    }
    Ok(())
}

fn gen_modrm_ins2<W : Write>(w: &mut W, op: &str, regs: &[&str], rmregs: &[&str], aregs: &[&str], sregs: &[&str], rev: bool) -> std::io::Result<()> {
    for s in regs {
        gen_modrm2(w, op, s, "", "%rip", aregs, sregs, true, rev)?;
    }
    for s in regs {
        gen_modrm2(w, op, s, "127", "%rbp", aregs, sregs, false, rev)?;
    }
    for s in regs {
        gen_modrm2(w, op, s, "128", "%rbp", aregs, sregs, false, rev)?;
    }
    if !rev {
        for s in regs {
            for d in rmregs {
                writeln!(w, " {op} {s}, {d}")?;
            }
        }
    }
    Ok(())
}

fn gen_0x00_to_0x3f() -> BDEResult<Vec<u8>> {
    let mut v = Vec::new();
    let w = &mut v;

    writeln!(w, " .text")?;
    for op in OPS {
        let opb = format!("{op}b");
        let opl = format!("{op}l");
        gen_modrm_ins2(w, &opb, &IREGS[4][0..8], &IREGS[4][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
        gen_modrm_ins2(w, &opl, &IREGS[2][0..8], &IREGS[2][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
        gen_modrm_ins2(w, &opb, &IREGS[4][0..8], &IREGS[4][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
        gen_modrm_ins2(w, &opl, &IREGS[2][0..8], &IREGS[2][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
        writeln!(w, " {opb} $1, %al")?;
        writeln!(w, " {opb} $127, %al")?;
        writeln!(w, " {opb} $-128, %al")?;
        writeln!(w, " {opb} $-1, %al")?;
    
        writeln!(w, " {opl} $-129, %eax")?;
        writeln!(w, " {opl} $128, %eax")?;
    }
    Ok(v)
}

fn gen_0x00_to_0x3f_with_0x66() -> BDEResult<Vec<u8>> {
    let mut v = Vec::new();
    let w = &mut v;

    writeln!(w, " .text")?;
    for op in OPS {
        let opw = format!("{op}w");
        gen_modrm_ins2(w, &opw, &IREGS[1][0..8], &IREGS[1][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
        gen_modrm_ins2(w, &opw, &IREGS[1][0..8], &IREGS[1][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
    
        writeln!(w, " {opw} $-32768, %ax")?;
        writeln!(w, " {opw} $-129, %ax")?;
        writeln!(w, " {opw} $128, %ax")?;
        writeln!(w, " {opw} $32767, %ax")?;
    }
    Ok(v)
}

fn gen_0x00_to_0x3f_with_0x48() -> BDEResult<Vec<u8>> {
    let mut v = Vec::new();
    let w = &mut v;

    writeln!(w, " .text")?;
    for op in OPS {
        let opq = format!("{op}q");
        gen_modrm_ins2(w, &opq, &IREGS[3][0..8], &IREGS[3][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;
        gen_modrm_ins2(w, &opq, &IREGS[3][0..8], &IREGS[3][0..8], &IREGS[3][0..8], &IREGS[3][0..8], true)?;
    
        writeln!(w, " {opq} $-32768, %rax")?;
        writeln!(w, " {opq} $-129, %rax")?;
        writeln!(w, " {opq} $128, %rax")?;
        writeln!(w, " {opq} $32767, %rax")?;
        writeln!(w, " {opq} $0x7fffffff, %rax")?;
    }
    Ok(v)
}

fn gen_0x50_to_0x6f() -> BDEResult<Vec<u8>> {
    let mut v = Vec::new();
    let w = &mut v;

    writeln!(w, " .text")?;
    for r in &IREGS[3][0..8] {
        writeln!(w, " pushq {r}")?;
    }
    for r in &IREGS[3][0..8] {
        writeln!(w, " popq {r}")?;
    }

    //     0000000000000000 <.text>:
    //    0:   63 c0                   movslq %eax,%eax
    //    2:   66 63 c0                movslq %eax,%ax
    //    5:   48 63 c0                movslq %eax,%rax
    // gen_modrm_ins(w, "movslq", &IREGS[2][0..8], &IREGS[3][0..8], &IREGS[3][0..8], &IREGS[3][0..8], false)?;

    writeln!(w, " addq %rax, %fs:0(%rax)")?;
    writeln!(w, " addq %rax, %fs:0")?;
    writeln!(w, " addq %rax, %fs:0(,%rax,2)")?;
    writeln!(w, " addq %rax, %fs:0(%rax,%rbx,2)")?;
    writeln!(w, " addq %rax, %gs:0(%rax)")?;
    writeln!(w, " addq %rax, %gs:0")?;
    writeln!(w, " addq %rax, %gs:0(,%rax,2)")?;
    writeln!(w, " addq %rax, %gs:0(%rax,%rbx,2)")?;

    writeln!(w, " pushq $0x10000")?;
    writeln!(w, " pushq $0x8000")?;
    // writeln!(w, " imull $0x80, (%rcx), %ebx")?;
    // writeln!(w, " imull $0x10000, (%rcx), %ebx")?;
    
    writeln!(w, " pushq $0x00")?;

    // writeln!(w, " imull $1, (%rcx), %ebx")?;
    // writeln!(w, " imull $-1, (%rcx), %ebx")?;

    Ok(v)
}

fn gen_0x80_to_0x8f() -> BDEResult<Vec<u8>> {
    let mut v = Vec::new();
    let w = &mut v;

    writeln!(w, " .text")?;
    writeln!(w, " jo .+0")?;
    writeln!(w, " jno .+0")?;
    writeln!(w, " jb .+0")?;
    writeln!(w, " jnae .+0")?;
    writeln!(w, " jc .+0")?;
    writeln!(w, " jnb .+0")?;
    writeln!(w, " jae .+0")?;
    writeln!(w, " jnc .+0")?;
    writeln!(w, " jz .+0")?;
    writeln!(w, " je .+0")?;
    writeln!(w, " jnz .+0")?;
    writeln!(w, " jne .+0")?;
    writeln!(w, " jbe .+0")?;
    writeln!(w, " jna .+0")?;
    writeln!(w, " jnbe .+0")?;
    writeln!(w, " ja .+0")?;
    writeln!(w, " js .+0")?;
    writeln!(w, " jns .+0")?;
    writeln!(w, " jp .+0")?;
    writeln!(w, " jpe .+0")?;
    writeln!(w, " jnp .+0")?;
    writeln!(w, " jpo .+0")?;
    writeln!(w, " jl .+0")?;
    writeln!(w, " jnge .+0")?;
    writeln!(w, " jnl .+0")?;
    writeln!(w, " jge .+0")?;
    writeln!(w, " jle .+0")?;
    writeln!(w, " jng .+0")?;
    writeln!(w, " jnle .+0")?;
    writeln!(w, " jg .+0")?;
    Ok(v)
}

fn test_against_gcc(text: Vec<u8>, name: &str) -> BDEResult<()> {
    let args = format!("-al --listing-lhs-width=3");
    let mut cmd = Command::new("x86_64-linux-gnu-as");
    let mut child = cmd
        .args(args.split_ascii_whitespace())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let child_stdin = child.stdin.as_mut().unwrap();
    child_stdin.write(&text)?;

    let output = child.wait_with_output()?;

    let text = std::str::from_utf8(&output.stdout)?;

    std::fs::write(format!("/tmp/{name}"), text)?;

    if !output.status.success() {
        info!("Compile error");
        for line in text.split('\n') {
            let mut words = line.split_ascii_whitespace();
            let Some(v) = words.next() else { continue };
            let Ok(_line_number) = v.parse::<u32>() else { continue };
            let Some(_src) = words.next() else { continue };
    
            let mut bytes : Vec<u8> = vec![];
            while let Some(src) = words.next() {
                if src.bytes().any(|b| b.is_ascii_lowercase()) {
                    break;
                } else if let Ok(w) = u32::from_str_radix(src, 16) {
                    let len = src.len();
                    bytes.extend(&w.to_be_bytes()[4-len/2..4]);
                } else {
                    break;
                }
            }
            if bytes.is_empty() {
                info!("fail: {line}");
            }
        }
        panic!("compile error");
    }

    let mut prev_bytes = EncodeResult::new();
    for line in text.split('\n') {
        if line.contains(".text") { continue; }
        if false {
            info!("{line}");
        }
        let mut words = line.split_ascii_whitespace();
        let Some(v) = words.next() else { continue };
        let Ok(_line_number) = v.parse::<u32>() else { continue };
        let Some(_src) = words.next() else { continue };
        // let Ok(_address) = u64::from_str_radix(src, 16) else { continue };

        let mut bytes : Vec<u8> = vec![];
        let mut asm = String::new();
        while let Some(src) = words.next() {
            if src.bytes().any(|b| b.is_ascii_lowercase()) {
                asm.push_str(src);
                break;
            } else if let Ok(w) = u32::from_str_radix(src, 16) {
                let len = src.len();
                bytes.extend(&w.to_be_bytes()[4-len/2..4]);
            } else {
                asm.push_str(src);
                break;
            }
        }

        while let Some(src) = words.next() {
            asm.push_str(" ");
            asm.push_str(src);
        }
    
        if asm.starts_with(".") {
            continue;
        }

        let ins = Instruction::from_str(&asm).expect(&asm);
        let obytes = ins.encode().expect(&asm);

        if obytes.as_slice().unwrap() != &bytes || obytes.as_slice() < prev_bytes.as_slice() {
            info!("gnu:     {:02x?} asm: {}", hex::encode(&bytes), asm);
            info!("miniasm: {:02x?} asm: {}", hex::encode(obytes.as_slice().unwrap()), ins);
            info!("prev: {}", hex::encode(prev_bytes.as_slice().unwrap()));
        }

        assert_eq!(obytes.as_slice().unwrap(), &bytes);

        // Generated instructions must be monotonic.
        assert!(obytes.as_slice() >= prev_bytes.as_slice());
        prev_bytes = obytes;
    }

    Ok(())
}

fn init_logger() {
    static START: std::sync::Once = std::sync::Once::new();
    START.call_once(|| {
        let _ = SimpleLogger::new().with_level(log::LevelFilter::Info).init();
    });
}

#[test]
fn test_0x00_to_0x3f() -> BDEResult<()> {
    init_logger();
    test_against_gcc(gen_0x00_to_0x3f()?, "0x00_to_0x3f")?;
    Ok(())
}

#[test]
fn test_0x00_to_0x3f_with_0x66() -> BDEResult<()> {
    init_logger();
    test_against_gcc(gen_0x00_to_0x3f_with_0x66()?, "0x00_to_0x3f_with_0x66")?;
    Ok(())
}

#[test]
fn test_0x00_to_0x3f_with_0x48() -> BDEResult<()> {
    init_logger();
    test_against_gcc(gen_0x00_to_0x3f_with_0x48()?, "0x00_to_0x3f_with_0x48")?;
    Ok(())
}

#[test]
fn test_0x50_to_0x6f() -> BDEResult<()> {
    init_logger();
    test_against_gcc(gen_0x50_to_0x6f()?, "0x50_to_0x6f")?;
    Ok(())
}

#[test]
fn test_0x80_to_0x8f() -> BDEResult<()> {
    init_logger();
    test_against_gcc(gen_0x80_to_0x8f()?, "0x80_to_0x8f")?;
    Ok(())
}

// for op in ops {
//     let add = format!("{op}b");
//     gen_modrm_ins1(w, &addb, &IREGS[4][0..8], &IREGS[3][0..8], &IREGS[3][0..8])?;
// }
