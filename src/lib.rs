#![no_std]
#![feature(fmt_internals)]

pub mod micro_string;
pub mod mico_vec;
pub use micro_string::{MicroString, ToMicroString};

pub mod x86 {
    #![allow(dead_code)]

    use core::fmt::Write;

    use crate::mico_vec::MicroVec;
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    /// Deliberately ignore these for now.
    /// AH, CH, DH, BH,
    pub enum Reg {
        // 8 Bit.
        AL, CL, DL, BL, SPL, BPL, SIL, DIL,
        R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,

        // 16 Bit.
        AX, CX, DX, BX, SP, BP, SI, DI,
        R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,

        // 32 Bit.
        EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,

        // 64 Bit.
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        R8, R9, R10, R11, R12, R13, R14, R15,

        // Special
        RIP,
        
        // Legacy
        AH, CH, DH, BH,
    }

    pub use Reg::*;
    pub static REGS : &[Reg] = &[
        // 8 Bit.
        AL, CL, DL, BL, SPL, BPL, SIL, DIL,
        R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,

        // 16 Bit.
        AX, CX, DX, BX, SP, BP, SI, DI,
        R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,

        // 32 Bit.
        EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,

        // 64 Bit.
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        R8, R9, R10, R11, R12, R13, R14, R15,
    ];

    pub static REGNAMES : &[&str] = &[
        // 8 Bit.
        "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil",
        "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",

        // 16 bit.
        "ax", "cx", "dx", "bx", "sp", "bp", "si", "di",
        "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",

        // 32 bit.
        "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
        "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",

        // 64 bit.
        "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",

    ];

    impl core::fmt::Display for Reg {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_char('%')?;
            f.write_str(REGNAMES[*self as u8 as usize])
        }
    }

    #[derive(Debug, PartialEq)]
    #[repr(u8)]
    pub enum RegClass {
        I8,
        I16,
        I32,
        I64,
        PC,
        H8,
    }

    #[derive(Debug, PartialEq)]
    #[repr(u8)]
    pub enum Scale {
        S1,
        S2,
        S4,
        S8,
    }

    pub enum Arg {
        R(Reg),
        I(i32),
        M(i32, Reg),
        Ms(i32, Reg, Scale),
        M2s(i32, Reg, Reg, Scale),
    }

    impl core::fmt::Display for Arg {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Arg::R(r) => r.fmt(f),
                Arg::I(v) => v.fmt(f),
                Arg::M(o, r1) => {
                    if *o == 0 { write!(f, "({r1})") } else { write!(f, "{o}({r1})") }
                }
                Arg::Ms(o, r2, scale) => {
                    if *o == 0 { write!(f, "(,{r2},{scale})") } else { write!(f, "{o}(,{r2},{scale})") }
                }
                Arg::M2s(o, r1, r2, scale) => {
                    if *o == 0 { write!(f, "({r1},{r2},{scale})") } else { write!(f, "{o}({r1},{r2},{scale})") }
                }
            }
        }
    }

    trait FmtArgs {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result;
    }

    impl FmtArgs for () {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result {
            write!(f, "{name}")
        }
    }

    impl FmtArgs for Arg {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result {
            write!(f, "{name} {}", &self)
        }
    }

    impl FmtArgs for (Arg, Arg) {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result {
            write!(f, "{name} {}, {}", &self.0, &self.1)
        }
    }

    macro_rules! insn {
        ($($id: ident ($args: tt), $name: expr, $params : expr,)*) => {
            pub enum Instruction {
                $(
                    $id($args),
                )*
            }

            impl Instruction {
                pub fn from_str(src: &str) -> Result<Instruction, ParseError> {
                    use Instruction::*;
                    match src.split_once(' ') {
                        $(
                            Some(($name, args)) => Ok($id(FromStr::from_str(args)?)),
                        )*
                        _=> Err(ParseError),
                    }
                }

                pub fn encode(&self) -> Result<EncodeResult, EncodeError> {
                    use Instruction::*;
                    use RegClass::*;
                    match self {
                        $(
                            $id(args) => (args, $params).encode(),
                        )*
                    }
                }
            }

            impl core::fmt::Display for Instruction {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    use Instruction::*;
                    match self {
                        $(
                            $id(args) => args.fmt_args(f, $name),
                        )*
                    }
                }
            }
        }
    }

    insn! {
        AddB((Arg, Arg)), "addb", (I8, 0x00, 0x00),
        AddW((Arg, Arg)), "addw", (I16, 0x01, 0x00),
        AddL((Arg, Arg)), "addl", (I32, 0x01, 0x00),
        AddQ((Arg, Arg)), "addq", (I64, 0x01, 0x00),
        RetQ(()), "retq", 0xc3,
    }

    impl Reg {
        // pub fn from_class_and_number(class: RegClass, number: u8) -> Result<Reg, &'static str> {
        //     if number >= 16 { return Err("from_class_and_number: register number too large"); }
        //     use Reg::*;
        //     use RegClass::*;
        //     match class {
        //         I8 => Ok([AL, CL, DL, BL, SPL, BPL, SIL, DIL,
        //             R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,][number as usize]),

        //         I16 => Ok([AX, CX, DX, BX, SP, BP, SI, DI,
        //             R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,][number as usize]),

        //         I32 => Ok([EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        //             R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,][number as usize]),

        //         I64 => Ok([RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        //             R8, R9, R10, R11, R12, R13, R14, R15,][number as usize]),
        //     }
        // }

        pub fn class(&self) -> RegClass {
            use Reg::*;
            use RegClass::*;
            match self {
                AL|CL|DL|BL|SPL|BPL|SIL|DIL|R8B|R9B|R10B|R11B|R12B|R13B|R14B|R15B => I8,
                AX|CX|DX|BX|SP|BP|SI|DI|R8W|R9W|R10W|R11W|R12W|R13W|R14W|R15W => I16,
                EAX|ECX|EDX|EBX|ESP|EBP|ESI|EDI|R8D|R9D|R10D|R11D|R12D|R13D|R14D|R15D => I32,
                RAX|RCX|RDX|RBX|RSP|RBP|RSI|RDI|R8|R9|R10|R11|R12|R13|R14|R15 => I64,
                RIP => PC,
                AH|CH|DH|BH => H8,
            }
        }

        pub fn number(&self) -> u8 {
            use Reg::*;
            match self {
                AL|CL|DL|BL|SPL|BPL|SIL|DIL|R8B|R9B|R10B|R11B|R12B|R13B|R14B|R15B => (*self as u8) & 0x0f,
                AX|CX|DX|BX|SP|BP|SI|DI|R8W|R9W|R10W|R11W|R12W|R13W|R14W|R15W => (*self as u8) & 0x0f,
                EAX|ECX|EDX|EBX|ESP|EBP|ESI|EDI|R8D|R9D|R10D|R11D|R12D|R13D|R14D|R15D => (*self as u8) & 0x0f,
                RAX|RCX|RDX|RBX|RSP|RBP|RSI|RDI|R8|R9|R10|R11|R12|R13|R14|R15 => (*self as u8) & 0x0f,
                RIP => 0,
                AH => 4,
                CH => 5,
                DH => 6,
                BH => 7,
            }
        }

    }

    pub type EncodeResult = MicroVec<u8, 15>;

    #[derive(Debug)]
    pub struct EncodeError;

    #[derive(Debug)]
    pub struct ParseError;

    pub trait Encode {
        fn encode(self) -> Result<EncodeResult, EncodeError>;
    }

    pub trait FromStr : Sized {
        fn from_str(src: &str) -> Result<Self, ParseError>;
    }

    impl Encode for (&(), u8) {
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let mut res = EncodeResult::default();
            res.push_sat(self.1);
            Ok(res)
        }
    }

    impl Encode for (&(Arg, Arg), (RegClass, u8, u8)) {
        #[inline(never)]
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let ((src, dest), (class, fwd, _back)) = self;
            use Arg::*;
            use RegClass::*;
            let mut res = EncodeResult::default();
            match (src, dest) {
                (R(s), R(d)) => {
                    if s.class() != class || d.class() != class { return Err(EncodeError) }

                    let (ns, rexr) = (s.number() & 0x07, s.number() >> 3);
                    let (nd, rexb) = (d.number() & 0x07, d.number() >> 3);
    
                    match class {
                        I8  => {
                            if ns >= 4 || nd >= 4 || rexr != 0 || rexb != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexb)
                            }
                        }
                        I16 => {
                            res.push_sat(0x66);
                            if rexr != 0 || rexb != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexb)
                            }
                        }
                        I32 => {
                            if rexr != 0 || rexb != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexb)
                            }
                        }
                        I64 => {
                            res.push_sat(0x48 + rexr * 4 + rexb)
                        }
                        _ => return Err(EncodeError),
                    }
                    res.push_sat(fwd);
                    res.push_sat(0xc0 + ns * 8 + nd);
                }
                (R(s), M(offset, a)) => {
                    if s.class() != class { return Err(EncodeError) }
                    let aclass = a.class();

                    let (ns, rexr) = (s.number() & 0x07, s.number() >> 3);
                    let (na, rexx) = (a.number() & 0x07, a.number() >> 3);
    
                    if aclass == RegClass::I32 {
                        res.push_sat(0x67);
                    }
                    match class {
                        I8  => {
                            if ns >= 4 || rexr != 0 || rexx != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexx * 2)
                            }
                        }
                        I16 => {
                            res.push_sat(0x66);
                            if rexr != 0 || rexx * 2 != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexx * 2)
                            }
                        }
                        I32 => {
                            if rexr != 0 || rexx * 2 != 0 {
                                res.push_sat(0x40 + rexr * 4 + rexx * 2)
                            }
                        }
                        I64 => {
                            res.push_sat(0x48 + rexr * 4 + rexx * 2)
                        }
                        _ => return Err(EncodeError),
                    }
                    res.push_sat(fwd);
                    if na == 4 {
                        // rsp and r12 use SIB
                        if *offset == 0 && na != 5 {
                            res.push_sat(0x04 + ns * 8);
                            res.push_sat(0x24);
                        } else if *offset >= -128 && *offset < 128 {
                            res.push_sat(0x44 + ns * 8);
                            res.push_sat(0x24);
                            res.push_sat((*offset & 0xff) as u8);
                        } else {
                            res.push_sat(0x84 + ns * 8);
                            res.push_sat(0x24);
                            res.push_slice(&(*offset).to_le_bytes()).unwrap();
                        }
                    } else {
                        if *offset == 0 && na != 5 {
                            res.push_sat(0x00 + ns * 8 + na);
                        } else if *offset >= -128 && *offset < 128 {
                            res.push_sat(0x40 + ns * 8 + na);
                            res.push_sat((*offset & 0xff) as u8);
                        } else {
                            res.push_sat(0x80 + ns * 8 + na);
                            res.push_slice(&(*offset).to_le_bytes()).unwrap();
                        }
                    }
                }
                _ => return Err(EncodeError),
            }
            Ok(res)
        }
    }

    impl FromStr for () {
        fn from_str(_src: &str) -> Result<Self, ParseError> {
            Ok(())
        }
    }

    impl FromStr for Arg {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            if src.is_empty() {
                return Err(ParseError);
            }
            match src.as_bytes()[0] {
                b'%' => {
                    Ok(Arg::R(Reg::from_str(src)?))
                }
                b'0'..=b'9'|b'('|b'-' => {
                    // 123(%ecx) (%ecx,%edx,2) (, %edx, 2)
                    let mut offset : i32 = 0;
                    let Some(lhs) = src[0..].bytes().position(|b| b == b'(') else {
                        return Err(ParseError);
                    };
                    if lhs != 0 {
                        let Ok(o) = parse_immediate(&src[0..lhs]) else {
                            return Err(ParseError);
                        };
                        offset = o;
                    }
                    let Some(rhs) = src[lhs+1..].bytes().position(|b| b == b')') else {
                        return Err(ParseError);
                    };
                    let mut s = src[lhs+1..lhs+1+rhs].split(',');
                    match (s.next(), s.next(), s.next(), s.next()) {
                        (Some(r1), None, _, _) => {
                            Ok(Arg::M(offset, Reg::from_str(r1)?))
                        }
                        (Some(""), Some(r2), Some(scale), None) => {
                            Ok(Arg::Ms(offset, Reg::from_str(r2)?, Scale::from_str(scale)?))
                        }
                        (Some(r1), Some(r2), Some(scale), None) => {
                            Ok(Arg::M2s(offset, Reg::from_str(r1)?, Reg::from_str(r2)?, Scale::from_str(scale)?))
                        }
                        _ => Err(ParseError)
                    }

                }
                _ => Err(ParseError)
            }
        }
    }

    impl FromStr for (Arg, Arg) {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            if let Some((src, dest)) = src.split_once(", ") {
                Ok((FromStr::from_str(src)?, FromStr::from_str(dest)?))
            } else {
                Err(ParseError)
            }
        }
    }

    impl FromStr for Reg {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            // %rax
            if src.as_bytes().first() == Some(&b'%') {
                if let Some(idx) = REGNAMES
                    .iter().position(|n| n == &&src[1..])
                {
                    return Ok(REGS[idx])
                }
            }
            Err(ParseError)
        }
    }

    impl FromStr for Scale {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            match src.parse::<u8>().map_err(|_| ParseError)? {
                1 => Ok(Scale::S1),
                2 => Ok(Scale::S2),
                4 => Ok(Scale::S4),
                8 => Ok(Scale::S8),
                _ => Err(ParseError),
            }
        }
    }

    impl core::fmt::Display for Scale {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            let scale = match self {
                Scale::S1 => 1,
                Scale::S2 => 2,
                Scale::S4 => 4,
                Scale::S8 => 8,
            };
            scale.fmt(f)
        }
    }

    fn parse_immediate(src: &str) -> Result<i32, ParseError> {
        if src.starts_with("0x") {
            i32::from_str_radix(&src[2..], 16).map_err(|_| ParseError)
        } else {
            src.parse::<i32>().map_err(|_| ParseError)
        }
    }
}

