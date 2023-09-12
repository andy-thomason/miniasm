#![no_std]
#![feature(fmt_internals)]

pub mod micro_string;
pub mod mico_vec;
pub use micro_string::{MicroString, ToMicroString};

pub mod x86 {
    #![allow(dead_code)]

    use core::fmt::Write;

    use crate::mico_vec::MicroVec;

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
        AddB((Arg, Arg)), "addb", (I8, 0x00, 0x02),
        AddW((Arg, Arg)), "addw", (I16, 0x01, 0x03),
        AddL((Arg, Arg)), "addl", (I32, 0x01, 0x03),
        AddQ((Arg, Arg)), "addq", (I64, 0x01, 0x03),
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
            let ((src, dest), (_class, fwd, rev)) = self;
            use Arg::*;
            let mut res = EncodeResult::default();
            match (src, dest) {
                (R(s), R(d)) => {
                    gen_modrm(&mut res, fwd, s, Some(d), 0, None, None, &Scale::S1)?;
                }
                (R(s), M(offset, a)) => {
                    gen_modrm(&mut res, fwd, s, None, *offset, Some(a), None, &Scale::S1)?;
                }
                (R(s), I(offset)) => {
                    gen_modrm(&mut res, fwd, s, None, *offset, None, None, &Scale::S1)?;
                }
                (R(s), Ms(offset, sr, scale)) => {
                    gen_modrm(&mut res, fwd, s, None, *offset, None, Some(sr), scale)?;
                }
                (R(s), M2s(offset, a, sr, scale)) => {
                    gen_modrm(&mut res, fwd, s, None, *offset, Some(a), Some(sr), scale)?;
                }
                // (I(_), R(_)) => todo!(),
                // (I(_), I(_)) => todo!(),
                // (I(_), M(_, _)) => todo!(),
                // (I(_), Ms(_, _, _)) => todo!(),
                // (I(_), M2s(_, _, _, _)) => todo!(),
                (M(offset, a), R(s)) => {
                    gen_modrm(&mut res, rev, s, None, *offset, Some(a), None, &Scale::S1)?;
                }
                // (M(_, _), I(_)) => todo!(),
                // (M(_, _), M(_, _)) => todo!(),
                // (M(_, _), Ms(_, _, _)) => todo!(),
                // (M(_, _), M2s(_, _, _, _)) => todo!(),
                (Ms(offset, sr, scale), R(s)) => {
                    gen_modrm(&mut res, rev, s, None, *offset, None, Some(sr), scale)?;
                }
                // (Ms(_, _, _), I(_)) => todo!(),
                // (Ms(_, _, _), M(_, _)) => todo!(),
                // (Ms(_, _, _), Ms(_, _, _)) => todo!(),
                // (Ms(_, _, _), M2s(_, _, _, _)) => todo!(),
                (M2s(offset, a, sr, scale), R(s)) => {
                    gen_modrm(&mut res, rev, s, None, *offset, Some(a), Some(sr), scale)?;
                }
                // (M2s(_, _, _, _), I(_)) => todo!(),
                // (M2s(_, _, _, _), M(_, _)) => todo!(),
                // (M2s(_, _, _, _), Ms(_, _, _)) => todo!(),
                // (M2s(_, _, _, _), M2s(_, _, _, _)) => todo!(),
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
                    let lhs = src[0..].bytes().position(|b| b == b'(').unwrap_or(src.len());
                    if lhs != 0 {
                        let Ok(o) = parse_immediate(&src[0..lhs]) else {
                            return Err(ParseError);
                        };
                        offset = o;
                    }
                    if lhs == src.len() {
                        return Ok(Arg::I(offset));
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


    // fn gen_prefix(res: &mut EncodeResult, class: RegClass, aclass: Option<RegClass>, ns: u8, nd: Option<u8>, na: Option<u8>, rexr: u8, rexx: u8, rexb: u8) -> Result<(), EncodeError>  {
    fn gen_modrm(res: &mut EncodeResult, opcode: u8, s: &Reg, d: Option<&Reg>, offset: i32, a: Option<&Reg>, sr: Option<&Reg>, scale: &Scale) -> Result<(), EncodeError>  {
        use RegClass::*;

        // d and a/sr are exclusive
        // TODO: more exclsivity checks.
        if d.is_some() && (a.is_some() || sr.is_some()) {
            return Err(EncodeError);
        }

        if a.map(Reg::class) == Some(I32) {
            res.push_sat(0x67);
        }

        enum Offsize {
            O0, O1, O4,
        }

        let (base, mut offsize) = if offset == 0 {
            (0x00, Offsize::O0)
        } else if offset >= -128 && offset < 128 {
            (0x40, Offsize::O1)
        } else {
            (0x80, Offsize::O4)
        };

        let sibupper : u8 = match scale {
            Scale::S1 => 0x00,
            Scale::S2 => 0x40,
            Scale::S4 => 0x80,
            Scale::S8 => 0xc0,
        };
    
        let sclass = s.class();
        let dclass = d.map(Reg::class);
        let aclass = a.map(Reg::class);
        let srclass = sr.map(Reg::class);

        let slownum = s.lownum();
        let dlownum = d.map(Reg::lownum).unwrap_or_default();
        let alownum = a.map(Reg::lownum).unwrap_or_default();
        let srlownum = sr.map(Reg::lownum).unwrap_or_default();

        // https://www.amd.com/content/dam/amd/en/documents/processor-tech-docs/programmer-references/40332.pdf
        // REX.W 3 0 = Default operand size 1 = 64-bit operand size
        // REX.R 2 1-bit (msb) extension of the ModRM reg field 1 , permitting access to 16 registers.
        // REX.X 1 1-bit (msb) extension of the SIB index field 1 , permitting access to 16 registers.
        // REX.B 0 1-bit (msb) extension of the ModRM r/m field 1 , SIB base field 1 , or opcode reg field, permitting access to 16 registers.

        let rexw = (s.class() == I64) as u8;
        let rexr = s.highnum();
        let rexb = d.map(Reg::highnum).or(a.map(Reg::highnum)).unwrap_or_default();
        let rexx = sr.map(Reg::highnum).unwrap_or_default();
        let rex = 0x40 + rexw * 8 + rexr * 4 + rexb * 2 + rexx;

        if aclass == Some(I32) {
            res.push_sat(0x67);
        }

        if sclass == I16 {
            res.push_sat(0x66);
        }

        if rex != 0x40 || sclass == I8 && slownum >= 4 || dclass == Some(I8) && dlownum >= 4 {
            res.push_sat(rex);
        }

        res.push_sat(opcode);

        match (sclass, dclass, aclass, srclass) {
            (s, None, Some(a), None) if s.is_int() && a == RegClass::PC => {
                res.push_sat(base + slownum * 8 + 5);
                offsize = Offsize::O4;
            }
            (s, None, Some(a), None) if s.is_int() && a.is_addr() => {
                if alownum != 4 || rexx == 1 {
                    res.push_sat(base + slownum * 8 + alownum);
                } else {
                    // xSP uses SIB.
                    res.push_sat(base + slownum * 8 + 4);
                    res.push_sat(sibupper + 4 * 8 + alownum);
                }
            }
            (s, Some(d), None, None) if s.is_8bit() && d.is_8bit() => {
                res.push_sat(0xc0 + slownum * 8 + dlownum);
            }
            (s, Some(d), None, None) if s.is_int() && s == d => {
                res.push_sat(0xc0 + slownum * 8 + dlownum);
            }
            (s, None, Some(a), Some(sr)) if s.is_int() && a.is_addr() && sr.is_addr() => {
                res.push_sat(base + slownum * 8 + 4);
                res.push_sat(sibupper + srlownum * 8 + alownum);
            }
            (s, None, None, Some(sr)) if s.is_int() && sr.is_addr() => {
                res.push_sat(base + slownum * 8 + 4);
                res.push_sat(sibupper + srlownum * 8 + 5);
                offsize = Offsize::O4;
            }
            (s, None, None, None) if s.is_int() => {
                res.push_sat(base + slownum * 8 + 4);
                res.push_sat(sibupper + 4 * 8 + 5);
                offsize = Offsize::O4;
            }
            _ => todo!(),
        }

        match offsize {
            Offsize::O0 => (),
            Offsize::O1 => res.push_sat((offset & 0xff) as u8),
            Offsize::O4 => res.push_slice(&(offset).to_le_bytes()).unwrap(),
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

