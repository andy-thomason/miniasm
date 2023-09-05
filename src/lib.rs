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
        I(i64),
        M(i64, Reg),
        Ms(i64, Reg, Scale),
        M2s(i64, Reg, Reg, Scale),
    }

    impl core::fmt::Display for Arg {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Arg::R(r) => r.fmt(f),
                Arg::I(v) => v.fmt(f),
                Arg::M(o, r1) => if *o == 0 { write!(f, "({})", r1) } else { write!(f, "{}({})", o, r1) },
                Arg::Ms(_, _, _) => todo!(),
                Arg::M2s(_, _, _, _) => todo!(),
            }
        }
    }

    trait FmtArgs {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result;
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
    }

    impl Reg {
        pub fn from_class_and_number(class: RegClass, number: u8) -> Result<Reg, &'static str> {
            if number >= 16 { return Err("from_class_and_number: register number too large"); }
            use Reg::*;
            use RegClass::*;
            match class {
                I8 => Ok([AL, CL, DL, BL, SPL, BPL, SIL, DIL,
                    R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,][number as usize]),

                I16 => Ok([AX, CX, DX, BX, SP, BP, SI, DI,
                    R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,][number as usize]),

                I32 => Ok([EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
                    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,][number as usize]),

                I64 => Ok([RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
                    R8, R9, R10, R11, R12, R13, R14, R15,][number as usize]),
            }
        }

        pub fn class(&self) -> RegClass {
            use Reg::*;
            use RegClass::*;
            match self {
                AL|CL|DL|BL|SPL|BPL|SIL|DIL|R8B|R9B|R10B|R11B|R12B|R13B|R14B|R15B => I8,
                AX|CX|DX|BX|SP|BP|SI|DI|R8W|R9W|R10W|R11W|R12W|R13W|R14W|R15W => I16,
                EAX|ECX|EDX|EBX|ESP|EBP|ESI|EDI|R8D|R9D|R10D|R11D|R12D|R13D|R14D|R15D => I32,
                RAX|RCX|RDX|RBX|RSP|RBP|RSI|RDI|R8|R9|R10|R11|R12|R13|R14|R15 => I64,
            }
        }

        pub fn number(&self) -> u8 {
            (*self as u8) & 0x0f
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

    impl Encode for (&(Arg, Arg), (RegClass, u8, u8)) {
        #[inline(never)]
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let ((src, dest), (class, fwd, _back)) = self;
            use Arg::*;
            let mut res = EncodeResult::default();
            match (src, dest) {
                (R(s), R(d)) => {
                    if s.class() != class || d.class() != class { return Err(EncodeError) }

                    let (ns, rexr) = (s.number() & 0x07, s.number() >> 3);
                    let (nd, rexb) = (d.number() & 0x07, d.number() >> 3);
                    // let rex = 0x40 + rexw * 8 + rexr * 4 + rexx * 2 + rexb;
    
                    if class == RegClass::I16 {
                        res.push_sat(0x66);
                    }
                    if class == RegClass::I32 || (rexb != 0 && rexr != 0) {
                        res.push_sat(0x40 + rexr * 4 + rexb);
                    }
                    res.push_sat(fwd);
                    res.push_sat(0xc0 + ns * 8 + nd);
                }
                (R(s), M(offset, d)) => {
                    if s.class() != class { return Err(EncodeError) }

                    let (ns, rexr) = (s.number() & 0x07, s.number() >> 3);
                    let (nd, rexx) = (d.number() & 0x07, d.number() >> 3);
                    // let rex = 0x40 + rexw * 8 + rexr * 4 + rexx * 2 + rexb;
    
                    if class == RegClass::I16 {
                        res.push_sat(0x66);
                    }
                    if class == RegClass::I32 || (rexx != 0 && rexr != 0) {
                        res.push_sat(0x40 + rexr * 4 + rexx * 2);
                    }
                    res.push_sat(fwd);
                    if *offset == 0 {
                        res.push_sat(0x00 + ns * 8 + nd);
                    } else {
                        res.push_sat(0x00 + ns * 8 + nd);
                    }
                }
                _ => return Err(EncodeError),
            }
            Ok(res)
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
                    let mut offset = 0;
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
                        (Some(r1), Some(r2), Some(scale), None) => {
                            Ok(Arg::M2s(offset, Reg::from_str(r1)?, Reg::from_str(r2)?, Scale::from_str(r1)?))
                        }
                        _ => Err(ParseError)
                    }

                }
                _ => Err(ParseError)
            }
        }
    }

    impl FromStr for Reg {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            // %rax
            if let Some(idx) = REGNAMES.iter().position(|n| n == &&src[1..]) {
                Ok(REGS[idx])
            } else {
                Err(ParseError)
            }
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

    fn parse_immediate(src: &str) -> Result<i64, ParseError> {
        if src.starts_with("0x") {
            i64::from_str_radix(&src[2..], 16).map_err(|_| ParseError)
        } else {
            src.parse::<i64>().map_err(|_| ParseError)
        }
    }
}

