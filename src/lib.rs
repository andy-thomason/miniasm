#![no_std]
#![feature(fmt_internals)]

pub mod micro_string;
pub mod encode_result;
pub use micro_string::{MicroString, ToMicroString};

pub mod x86 {
    #![allow(dead_code)]

    use core::fmt::Write;
    use crate::encode_result::EncodeResult;

    macro_rules! def_regs {
        ($(($id : ident, $name : expr, $number : expr),)*) => {
            #[repr(u8)]
            #[derive(Debug, Clone, Copy)]
            pub enum Reg {
                $($id),*
            }

            pub static REGS : &[Reg] = &[
                $(Reg::$id),*
            ];

            pub static REGNAMES : &[&str] = &[
                $($name),*
            ];

            pub static REGNUMS : &[u8] = &[
                $($number),*
            ];

        }
    }

    def_regs! {
        // 8 Bit.
        (AL, "al", 0), (CL, "cl", 1), (DL, "dl", 2), (BL, "bl", 3),
        (SPL, "spl", 4), (BPL, "bpl", 5), (SIL, "sil", 6), (DIL, "dil", 7),
        (R8B, "r8b", 8), (R9B, "r9b", 9), (R10B, "r10b", 10), (R11B, "r11b", 11),
        (R12B, "r12b", 12), (R13B, "r13b", 13), (R14B, "r14b", 14), (R15B, "r15b", 15), 

        // 16 Bit.
        (AX, "ax", 0), (CX, "cx", 1), (DX, "dx", 2), (BX, "bx", 3),
        (SP, "sp", 4), (BP, "bp", 5), (SI, "si", 6), (DI, "di", 7),
        (R8W, "r8w", 8), (R9W, "r9w", 9), (R10W, "r10w", 10), (R11W, "r11w", 11),
        (R12W, "r12w", 12), (R13W, "r13w", 13), (R14W, "r14w", 14), (R15W, "r15w", 15), 

        // 32 Bit.
        (EAX, "eax", 0), (ECX, "ecx", 1), (EDX, "edx", 2), (EBX, "ebx", 3),
        (ESP, "esp", 4), (EBP, "ebp", 5), (ESI, "esi", 6), (EDI, "edi", 7),
        (R8D, "r8d", 8), (R9D, "r9d", 9), (R10D, "r10d", 10), (R11D, "r11d", 11),
        (R12D, "r12d", 12), (R13D, "r13d", 13), (R14D, "r14d", 14), (R15D, "r15d", 15), 

        // 64 Bit.
        (RAX, "rax", 0), (RCX, "rcx", 1), (RDX, "rdx", 2), (RBX, "rbx", 3),
        (RSP, "rsp", 4), (RBP, "rbp", 5), (RSI, "rsi", 6), (RDI, "rdi", 7),
        (R8, "r8", 8), (R9, "r9", 9), (R10, "r10", 10), (R11, "r11", 11),
        (R12, "r12", 12), (R13, "r13", 13), (R14, "r14", 14), (R15, "r15", 15), 

        // Special
        (RIP, "rip", 0),

        // Legacy
        (AH, "ah", 4), (CH, "ch", 5), (DH, "dh", 6), (BH, "bh", 7), 
    }


    impl core::fmt::Display for Reg {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_char('%')?;
            f.write_str(REGNAMES[*self as u8 as usize])
        }
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
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
    pub enum Category {
        OpAddSub,
        OpPushPop,
        OpImul,
    }

    #[derive(Debug, PartialEq)]
    #[repr(u8)]
    pub enum Scale {
        S1,
        S2,
        S4,
        S8,
    }

    const LABEL_MAX : usize = 16;
    pub struct Label([u8; LABEL_MAX]);

    enum Offsize {
        O0, O1, O4,
    }

    pub enum Arg {
        R(Reg),
        I(i32),
        M(i32),
        Mr(i32, Reg),
        Ms(i32, Reg, Scale),
        M2s(i32, Reg, Reg, Scale),
    }

    impl core::fmt::Display for Arg {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Arg::R(r) => r.fmt(f),
                Arg::I(v) => write!(f, "${v}"),
                Arg::M(v) => v.fmt(f),
                Arg::Mr(o, r1) => {
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

    impl FmtArgs for Label {
        fn fmt_args(&self, f: &mut core::fmt::Formatter, name: &str) ->  core::fmt::Result {
            write!(f, "{name} {}", core::str::from_utf8(&self.0).unwrap())
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

    // let ops = ["add", "or", "adc", "sbb", "and", "sub", "xor"];

    insn! {
        AddB((Arg, Arg)), "addb", (I8, 0, Category::OpAddSub),
        AddW((Arg, Arg)), "addw", (I16, 0, Category::OpAddSub),
        AddL((Arg, Arg)), "addl", (I32, 0, Category::OpAddSub),
        AddQ((Arg, Arg)), "addq", (I64, 0, Category::OpAddSub),
        OrB((Arg, Arg)), "orb", (I8, 1, Category::OpAddSub),
        OrW((Arg, Arg)), "orw", (I16, 1, Category::OpAddSub),
        OrL((Arg, Arg)), "orl", (I32, 1, Category::OpAddSub),
        OrQ((Arg, Arg)), "orq", (I64, 1, Category::OpAddSub),
        AdcB((Arg, Arg)), "adcb", (I8, 2, Category::OpAddSub),
        AdcW((Arg, Arg)), "adcw", (I16, 2, Category::OpAddSub),
        AdcL((Arg, Arg)), "adcl", (I32, 2, Category::OpAddSub),
        AdcQ((Arg, Arg)), "adcq", (I64, 2, Category::OpAddSub),
        SbbB((Arg, Arg)), "sbbb", (I8, 3, Category::OpAddSub),
        SbbW((Arg, Arg)), "sbbw", (I16, 3, Category::OpAddSub),
        SbbL((Arg, Arg)), "sbbl", (I32, 3, Category::OpAddSub),
        SbbQ((Arg, Arg)), "sbbq", (I64, 3, Category::OpAddSub),
        AndB((Arg, Arg)), "andb", (I8, 4, Category::OpAddSub),
        AndW((Arg, Arg)), "andw", (I16, 4, Category::OpAddSub),
        AndL((Arg, Arg)), "andl", (I32, 4, Category::OpAddSub),
        AndQ((Arg, Arg)), "andq", (I64, 4, Category::OpAddSub),
        SubB((Arg, Arg)), "subb", (I8, 5, Category::OpAddSub),
        SubW((Arg, Arg)), "subw", (I16, 5, Category::OpAddSub),
        SubL((Arg, Arg)), "subl", (I32, 5, Category::OpAddSub),
        SubQ((Arg, Arg)), "subq", (I64, 5, Category::OpAddSub),
        XorB((Arg, Arg)), "xorb", (I8, 6, Category::OpAddSub),
        XorW((Arg, Arg)), "xorw", (I16, 6, Category::OpAddSub),
        XorL((Arg, Arg)), "xorl", (I32, 6, Category::OpAddSub),
        XorQ((Arg, Arg)), "xorq", (I64, 6, Category::OpAddSub),
        CmpB((Arg, Arg)), "cmpb", (I8, 7, Category::OpAddSub),
        CmpW((Arg, Arg)), "cmpw", (I16, 7, Category::OpAddSub),
        CmpL((Arg, Arg)), "cmpl", (I32, 7, Category::OpAddSub),
        CmpQ((Arg, Arg)), "cmpq", (I64, 7, Category::OpAddSub),
        PushQ(Arg), "pushq", (I64, 0, Category::OpPushPop),
        PopQ(Arg), "popq", (I64, 1, Category::OpPushPop),
        //IMulL((Arg, Arg, Arg)), "imull", (I32, 0, Category::OpImul),
        Jo(Label), "jo", 0x70,
        Jno(Label), "jno", 0x71,
        Jb(Label), "jb", 0x72,
        Jnae(Label), "jnae", 0x72,
        Jc(Label), "jc", 0x72,
        Jnb(Label), "jnb", 0x73,
        Jae(Label), "jae", 0x73,
        Jnc(Label), "jnc", 0x73,
        Jz(Label), "jz", 0x74,
        Je(Label), "je", 0x74,
        Jnz(Label), "jnz", 0x75,
        Jne(Label), "jne", 0x75,
        Jbe(Label), "jbe", 0x76,
        Jna(Label), "jna", 0x76,
        Jnbe(Label), "jnbe", 0x77,
        Ja(Label), "ja", 0x77,
        Js(Label), "js", 0x78,
        Jns(Label), "jns", 0x79,
        Jp(Label), "jp", 0x7A,
        Jpe(Label), "jpe", 0x7A,
        Jnp(Label), "jnp", 0x7B,
        Jpo(Label), "jpo", 0x7B,
        Jl(Label), "jl", 0x7C,
        Jnge(Label), "jnge", 0x7C,
        Jnl(Label), "jnl", 0x7D,
        Jge(Label), "jge", 0x7D,
        Jle(Label), "jle", 0x7E,
        Jng(Label), "jng", 0x7E,
        Jnle(Label), "jnle", 0x7F,
        Jg(Label), "jg", 0x7F,
        RetQ(()), "retq", 0xc3,
    }

    impl Reg {
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
            REGNUMS[*self as u8 as usize]
        }

        fn lownum(&self) -> u8 {
            self.number() & 7
        }

        fn highnum(&self) -> u8 {
            self.number() >> 3
        }
    }

    impl RegClass {
        fn is_8bit(&self) -> bool {
            use RegClass::*;
            match &self {
                I8|H8 => true,
                _ => false,
            }
        }

        fn is_int(&self) -> bool {
            use RegClass::*;
            match &self {
                I8|H8|I16|I32|I64 => true,
                _ => false,
            }
        }

        fn is_addr(&self) -> bool {
            use RegClass::*;
            match &self {
                I32|I64 => true,
                _ => false,
            }
        }
    }

    // pub type EncodeResult = MicroVec<u8, 15>;

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
            res.push_u8(self.1);
            Ok(res)
        }
    }

    impl Encode for (&Label, u8) {
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let mut res = EncodeResult::default();
            res.push_u8(self.1);
            Ok(res)
        }
    }

    impl Encode for (&Arg, (RegClass, u8, Category)) {
        #[inline(never)]
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let (dest, (class, group, category)) = self;
            use Arg::*;
            let mut res = EncodeResult::default();
            match dest {
                R(rm) => {
                    generic_ins(&mut res, class, group, category, None, Some(rm), None, None, None, &Scale::S1, None, false)?;
                }
                M(imm) => {
                    generic_ins(&mut res, class, group, category, None, None, Some(*imm), None, None, &Scale::S1, None, false)?;
                }
                Mr(offset, a) => {
                    generic_ins(&mut res, class, group, category, None, None, Some(*offset), Some(a), None, &Scale::S1, None, false)?;
                }
                Ms(offset, sr, scale) => {
                    generic_ins(&mut res, class, group, category, None, None, Some(*offset), None, Some(sr), scale, None, false)?;
                }
                M2s(offset, a, sr, scale) => {
                    generic_ins(&mut res, class, group, category, None, None, Some(*offset), Some(a), Some(sr), scale, None, false)?;
                }
                I(immediate) => {
                    generic_ins(&mut res, class, group, category, None, None, None, None, None, &Scale::S1, Some(*immediate), false)?;
                }
                _ => todo!(),
            }
            Ok(res)
        }
    }

    // /// Encoding for add, sub etc. in one of 8 groups.
    // impl Encode for (&(Arg, Arg), (RegClass, u8)) {
    //     #[inline(never)]
    //     fn encode(self) -> Result<EncodeResult, EncodeError> {
    //         let ((src, dest), (class, group)) = self;
    //         use Arg::*;
    //         let mut res = EncodeResult::default();
    //         match (src, dest) {
    //             (R(r), R(rm)) => {
    //                 generic_ins(&mut res, class, group, Category::OpFwd, Some(r), Some(rm), None, None, None, &Scale::S1, None)?;
    //             }
    //             (R(r), M(imm)) => {
    //                 generic_ins(&mut res, class, group, Category::OpFwd, Some(r), None, Some(*imm), None, None, &Scale::S1, None)?;
    //             }
    //             (R(r), Mr(offset, a)) => {
    //                 generic_ins(&mut res, class, group, Category::OpFwd, Some(r), None, Some(*offset), Some(a), None, &Scale::S1, None)?;
    //             }
    //             (R(r), Ms(offset, sr, scale)) => {
    //                 generic_ins(&mut res, class, group, Category::OpFwd, Some(r), None, Some(*offset), None, Some(sr), scale, None)?;
    //             }
    //             (R(r), M2s(offset, a, sr, scale)) => {
    //                 generic_ins(&mut res, class, group, Category::OpFwd, Some(r), None, Some(*offset), Some(a), Some(sr), scale, None)?;
    //             }
    //             (I(immediate), R(rm)) => {
    //                 generic_ins(&mut res, class, group, Category::OpImm8, None, Some(rm), None, None, None, &Scale::S1, Some(*immediate))?;
    //             }
    //             // (I(_), M(_, _)) => todo!(),
    //             // (I(_), Ms(_, _, _)) => todo!(),
    //             // (I(_), M2s(_, _, _, _)) => todo!(),
    //             (Mr(offset, a), R(r)) => {
    //                 generic_ins(&mut res, class, group, Category::OpRev, Some(r), None, Some(*offset), Some(a), None, &Scale::S1, None)?;
    //             }
    //             (Ms(offset, sr, scale), R(r)) => {
    //                 generic_ins(&mut res, class, group, Category::OpRev, Some(r), None, Some(*offset), None, Some(sr), scale, None)?;
    //             }
    //             (M2s(offset, a, sr, scale), R(r)) => {
    //                 generic_ins(&mut res, class, group, Category::OpRev, Some(r), None, Some(*offset), Some(a), Some(sr), scale, None)?;
    //             }
    //             _ => todo!(),
    //         }
    //         Ok(res)
    //     }
    // }

    /// Encoding for two arg ops.
    impl Encode for (&(Arg, Arg), (RegClass, u8, Category)) {
        #[inline(never)]
        fn encode(self) -> Result<EncodeResult, EncodeError> {
            let ((src, dest), (class, group, category)) = self;
            use Arg::*;
            let mut res = EncodeResult::default();
            match (src, dest) {
                (R(r), R(rm)) => {
                    generic_ins(&mut res, class, group, category, Some(r), Some(rm), None, None, None, &Scale::S1, None, false)?;
                }
                (R(r), M(imm)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*imm), None, None, &Scale::S1, None, false)?;
                }
                (R(r), Mr(offset, a)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), Some(a), None, &Scale::S1, None, false)?;
                }
                (R(r), Ms(offset, sr, scale)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), None, Some(sr), scale, None, false)?;
                }
                (R(r), M2s(offset, a, sr, scale)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), Some(a), Some(sr), scale, None, false)?;
                }
                (I(immediate), R(rm)) => {
                    generic_ins(&mut res, class, group, category, None, Some(rm), None, None, None, &Scale::S1, Some(*immediate), false)?;
                }
                // (I(_), M(_, _)) => todo!(),
                // (I(_), Ms(_, _, _)) => todo!(),
                // (I(_), M2s(_, _, _, _)) => todo!(),
                (Mr(offset, a), R(r)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), Some(a), None, &Scale::S1, None, true)?;
                }
                (Ms(offset, sr, scale), R(r)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), None, Some(sr), scale, None, true)?;
                }
                (M2s(offset, a, sr, scale), R(r)) => {
                    generic_ins(&mut res, class, group, category, Some(r), None, Some(*offset), Some(a), Some(sr), scale, None, true)?;
                }
                _ => todo!(),
            }
            Ok(res)
        }
    }

    impl FromStr for () {
        fn from_str(_src: &str) -> Result<Self, ParseError> {
            Ok(())
        }
    }

    impl FromStr for Label {
        fn from_str(src: &str) -> Result<Self, ParseError> {
            let mut res = [0; LABEL_MAX];
            if src.len() > LABEL_MAX {
                Err(ParseError)
            } else {
                res[0..src.len()].copy_from_slice(src.as_bytes());
                Ok(Label(res))
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
                b'$' => {
                    let Ok(imm) = parse_immediate(&src[1..]) else {
                        return Err(ParseError);
                    };
                    Ok(Arg::I(imm))
                }
                b'0'..=b'9'|b'('|b'-' => {
                    // 123(%ecx) (%ecx,%edx,2) (, %edx, 2)
                    let mut offset : i32 = 0;
                    let lhs = src[0..].bytes().position(|b| b == b'(').unwrap_or(src.len());
                    if lhs != 0 {
                        let Ok(o) = parse_immediate(&src[0..lhs]) else {
                            return Err(ParseError);
                        };
                        offset = o;
                    }
                    if lhs == src.len() {
                        return Ok(Arg::M(offset));
                    }
                    let Some(rhs) = src[lhs+1..].bytes().position(|b| b == b')') else {
                        return Err(ParseError);
                    };
                    let mut s = src[lhs+1..lhs+1+rhs].split(',');
                    match (s.next(), s.next(), s.next(), s.next()) {
                        (Some(r1), None, _, _) => {
                            Ok(Arg::Mr(offset, Reg::from_str(r1)?))
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


    fn generic_ins(res: &mut EncodeResult, class: RegClass, group: u8, category: Category, r: Option<&Reg>, rm: Option<&Reg>, offset: Option<i32>, a: Option<&Reg>, sr: Option<&Reg>, scale: &Scale, immediate: Option<i32>, rev: bool) -> Result<(), EncodeError> {
        use RegClass::*;

        // rm and a/sr are exclusive
        // TODO: more exclsivity checks.
        if rm.is_some() && (a.is_some() || sr.is_some()) {
            return Err(EncodeError);
        }

        let rclass = r.map(Reg::class);
        let rmclass = rm.map(Reg::class);
        let aclass = a.map(Reg::class);
        let srclass = sr.map(Reg::class);

        let rlownum = r.map(Reg::lownum).unwrap_or_default();
        let rmlownum = rm.map(Reg::lownum).unwrap_or_default();
        // let alownum = a.map(Reg::lownum).unwrap_or_default();
        // let srlownum = sr.map(Reg::lownum).unwrap_or_default();
        let rhighnum = r.map(Reg::highnum).unwrap_or_default();

        // If %spl is used instead of %ah, use an 0x40 rex prefix anyway.
        use Reg::*;
        let is_spl_and_friends = 
            matches!(r, Some(SPL)|Some(BPL)|Some(SIL)|Some(DIL)) ||
            matches!(rm, Some(SPL)|Some(BPL)|Some(SIL)|Some(DIL));

        match category {
            Category::OpAddSub => {
                if !rev {
                    if let Some(immediate) = immediate {
                        match rm {
                            Some(Reg::AL) => if immediate >= -128 && immediate < 128 {
                                res.push_u8(group * 8 + 4);
                                res.push_i8(immediate.try_into().map_err(|_| EncodeError)?);
                            }
                            Some(Reg::EAX) => if immediate <= -129 || immediate >= 128 {
                                res.push_u8(group * 8 + 5);
                                res.push_i32(immediate.try_into().map_err(|_| EncodeError)?);
                                return Ok(());
                            }
                            _ => {
                                gen_prefix(res, class, rhighnum, rm, a, sr, is_spl_and_friends)?;
        
                                match class {
                                    I8|H8 => res.push_u8(0x80 + group),
                                    I32 => res.push_u8(0x83 + group),
                                    _ => todo!(),
                                }
                                
                                gen_modrm(res, rlownum, class, rm, a, sr, offset, scale)?;
                                res.push_i8(immediate.try_into().map_err(|_| EncodeError)?);
                            }
                        }
                    } else {
                        gen_prefix(res, class, rhighnum, rm, a, sr, is_spl_and_friends)?;
                        if matches!(rclass, Some(I8)|Some(H8)) {
                            res.push_u8(group * 8 + 0);
                        } else {
                            res.push_u8(group * 8 + 1);
                        }
                        gen_modrm(res, rlownum, class, rm, a, sr, offset, scale)?;
                    }
                } else {
                    gen_prefix(res, class, rhighnum, rm, a, sr, is_spl_and_friends)?;
                    if matches!(rclass, Some(I8)|Some(H8)) {
                        res.push_u8(group * 8 + 2);
                    } else {
                        res.push_u8(group * 8 + 3);
                    }
                    gen_modrm(res, rlownum, class, rm, a, sr, offset, scale)?;
                }
            }
            Category::OpPushPop => {
                if let Some(immediate) = immediate {
                    if group != 0 { return Err(EncodeError); }
                    if immediate >= 0 && immediate < 256 {
                        res.push_u8(0x6a);
                        res.push_u8(immediate.try_into().map_err(|_| EncodeError)?);
                    } else {
                        res.push_u8(0x68);
                        res.push_i32(immediate);
                    }
                } else if matches!((rmclass, aclass, srclass), (Some(I64), None, None)) {
                    res.push_u8(0x50 + group * 8 + rmlownum);
                } else {
                    todo!();
                }
            },
            Category::OpImul => {
                if let Some(immediate) = immediate {
                    if immediate >= 0 && immediate < 256 {
                        res.push_u8(0x6b);
                        res.push_u8(immediate.try_into().map_err(|_| EncodeError)?);
                    } else {
                        res.push_u8(0x69);
                        res.push_i32(immediate);
                    }
                } else {
                    todo!();
                }
            }
        }

        Ok(())
    }

    fn gen_prefix(res: &mut EncodeResult, class: RegClass, rhighnum: u8, rm: Option<&Reg>, a: Option<&Reg>, sr: Option<&Reg>, is_spl_and_friends: bool) -> Result<(), EncodeError> {
        let aclass = a.map(Reg::class);

        // https://www.amd.com/content/dam/amd/en/documents/processor-tech-docs/programmer-references/40332.pdf
        // REX.W 3 0 = Default operand size 1 = 64-bit operand size
        // REX.R 2 1-bit (msb) extension of the ModRM reg field 1 , permitting access to 16 registers.
        // REX.X 1 1-bit (msb) extension of the SIB index field 1 , permitting access to 16 registers.
        // REX.B 0 1-bit (msb) extension of the ModRM r/m field 1 , SIB base field 1 , or opcode reg field, permitting access to 16 registers.
        use RegClass::*;

        let rexw = (class == I64) as u8;
        let rexr = rhighnum;
        let rexb = rm.map(Reg::highnum).or(a.map(Reg::highnum)).unwrap_or_default();
        let rexx = sr.map(Reg::highnum).unwrap_or_default();
        let rex = 0x40 + rexw * 8 + rexr * 4 + rexb * 2 + rexx;

        if aclass == Some(I32) {
            res.push_u8(0x67);
        }

        if class == I16 {
            res.push_u8(0x66);
        }



        if rex != 0x40 || is_spl_and_friends {
            res.push_u8(rex);
        }

        Ok(())
    }

    fn gen_modrm(res: &mut EncodeResult, rlownum: u8, class: RegClass, rm: Option<&Reg>, a: Option<&Reg>, sr: Option<&Reg>, offset: Option<i32>, scale: &Scale) -> Result<(), EncodeError> {
        let anum = a.map(Reg::number).unwrap_or_default();
        let r2lownum = rm.map(Reg::lownum).unwrap_or_default();
        let alownum = a.map(Reg::lownum).unwrap_or_default();
        let srlownum = sr.map(Reg::lownum).unwrap_or_default();

        let sibupper : u8 = match scale {
            Scale::S1 => 0x00,
            Scale::S2 => 0x40,
            Scale::S4 => 0x80,
            Scale::S8 => 0xc0,
        };
    
        let (base, mut offsize) = if let Some(offset) = offset {
            if a.is_none() && sr.is_none() {
                (0x00 + rlownum * 8, Offsize::O4)
            } else if offset == 0 {
                (0x00 + rlownum * 8, Offsize::O0)
            } else if offset >= -128 && offset < 128 {
                (0x40 + rlownum * 8, Offsize::O1)
            } else {
                (0x80 + rlownum * 8, Offsize::O4)
            }
        } else {
            (0xc0 + rlownum * 8, Offsize::O0)
        };

        let rmclass = rm.map(Reg::class);
        let aclass = a.map(Reg::class);
        let srclass = sr.map(Reg::class);
        match (rmclass, aclass, srclass) {
            (None, Some(ac), None) if class.is_int() && ac == RegClass::PC => {
                res.push_u8(base + 5);
                offsize = Offsize::O4;
            }
            (None, Some(ac), None) if class.is_int() && ac.is_addr() => {
                if anum != 4 {
                    // not SP
                    res.push_u8(base + alownum);
                } else {
                    // SP uses SIB.
                    res.push_u8(base + 4);
                    res.push_u8(sibupper + 4 * 8 + alownum);
                }
            }
            (Some(rm), None, None) if class.is_8bit() && rm.is_8bit() => {
                res.push_u8(base + r2lownum);
            }
            (Some(rm), None, None) if class.is_int() && rm == class => {
                res.push_u8(base + r2lownum);
            }
            (None, Some(a), Some(sr)) if class.is_int() && a.is_addr() && sr.is_addr() => {
                res.push_u8(base + 4);
                res.push_u8(sibupper + srlownum * 8 + alownum);
            }
            (None, None, Some(sr)) if class.is_int() && sr.is_addr() => {
                res.push_u8(base + 4);
                res.push_u8(sibupper + srlownum * 8 + 5);
                offsize = Offsize::O4;
            }
            (None, None, None) if class.is_int() => {
                res.push_u8(base + 4);
                res.push_u8(sibupper + 4 * 8 + 5);
                offsize = Offsize::O4;
            }
            _ => todo!(),
        }

        if let Some(offset) = offset {
            match offsize {
                Offsize::O0 => (),
                Offsize::O1 => res.push_u8((offset & 0xff) as u8),
                Offsize::O4 => res.push_i32(offset),
            }
        }
        Ok(())
    }

    fn parse_immediate(src: &str) -> Result<i32, ParseError> {
        if src.starts_with("0x") {
            i32::from_str_radix(&src[2..], 16).map_err(|_| ParseError)
        } else {
            src.parse::<i32>().map_err(|_| ParseError)
        }
    }
}

