

#[derive(Default)]
pub struct EncodeResult {
    buf: [u8; 16],
    len: usize,
    overflow: bool,
}

pub enum Error {
    TooBig,
}

macro_rules! impl_push {
    ($(($push_name : ident, $type : ty)),*) => {
        $(
            pub fn $push_name(&mut self, val: $type) {
                let size = core::mem::size_of_val(&val);
                if self.len + size < self.buf.len() {
                    self.buf[self.len..self.len+size].copy_from_slice(&val.to_le_bytes());
                    self.len += size;
                } else {
                    self.overflow = true;
                }
            }
        )*
    }
}

impl EncodeResult {
    pub fn new() -> Self {
        Self::default()
    }

    impl_push!(
        (push_i8, i8), (push_i16, i16), (push_i32, i32),
        (push_u8, u8), (push_u16, u16), (push_u32, u32)
    );

    pub fn as_slice(&self) -> Option<&[u8]> {
        if self.overflow {
            None
        } else {
            Some(&self.buf[0..self.len])
        }
    }
}

