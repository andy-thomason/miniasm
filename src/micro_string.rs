
/// Pascal-style strings.
pub struct MicroString<const N : usize>([u8; N]);

impl<const N : usize> Default for MicroString<N> {
    fn default() -> Self {
        Self([0; N])
    }
}

impl<const N : usize> MicroString<N> {
    pub fn new(s: &str) -> Result<MicroString<N>, &'static str> {
        if s.len()+1 < N && N < 256 {
            let mut bytes = [0; N];
            bytes[0] = s.len() as u8;
            bytes[1..1+s.len()].copy_from_slice(s.as_bytes());
            Ok(MicroString(bytes))
        } else {
            Err("MicroString string too long.")
        }
    }

    pub fn as_str(&self) -> &str {
        // Safety: new enforces UTF8.
        unsafe {
            core::str::from_utf8_unchecked(&self.0[1..1+self.0[0] as usize])
        }
    }
}

impl<const N : usize> core::ops::Deref for MicroString<N> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<const N : usize> core::fmt::Write for MicroString<N> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let len = self.len();
        if 1 + len + s.len() > N {
            Err(core::fmt::Error)
        } else {
            self.0[1 + len..1 + len + s.len()].copy_from_slice(s.as_bytes());
            self.0[0] += s.len() as u8;
            Ok(())
        }
    }
}

pub trait ToMicroString : core::fmt::Display {
    fn to_micro_string<const N : usize>(&self) -> Result<MicroString<N>, core::fmt::Error> {
        let mut ms = MicroString::<N>::default();
        let mut f = core::fmt::Formatter::new(&mut ms);
        self.fmt(& mut f)?;
        Ok(ms)
    }
}

impl <T : core::fmt::Display> ToMicroString for T {}

#[test]
fn test_ms() {
    assert_eq!("xyz".to_micro_string::<8>().unwrap().as_str(), "xyz");
    assert_eq!(123.to_micro_string::<8>().unwrap().as_str(), "123");
}