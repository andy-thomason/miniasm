
#[derive(Debug)]
pub struct MicroVec<T, const N : usize>(u8, [T; N]);

#[derive(Debug)]
pub struct MicroVecError;

impl<T : Default + Copy, const N : usize> MicroVec<T, N> {
    #[inline]
    pub fn new() -> Self {
        let n = N;
        assert!(n < 256);
        Self(0, [T::default(); N])
    }
}

impl<T : Default + Copy, const N : usize> Default for MicroVec<T, N> {
    fn default() -> Self {
        Self(Default::default(), [Default::default(); N])
    }
}

impl<T : Default + Copy, const N : usize, const M : usize> From<[T; M]> for MicroVec<T, N> {
    fn from(value: [T; M]) -> Self {
        assert!(M <= N && M < 256);
        let mut val = [T::default(); N];
        val[0..M].copy_from_slice(&value);
        Self(M as u8, val)
    }
}

impl<T : Copy, const N : usize> MicroVec<T, N> {
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        let len = self.0 as usize;
        &self.1[0..len]
    }

    #[inline]
    pub fn push(&mut self, t: T) -> Result<(), MicroVecError> {
        let len = self.0 as usize;
        if len + 1 > N {
            Err(MicroVecError)
        } else {
            self.1[len] = t;
            self.0 += 1;
            Ok(())
        }
    }

    #[inline]
    pub fn push_sat(&mut self, t: T) {
        let len = self.0 as usize;
        if len + 1 < N {
            self.1[len] = t;
            self.0 += 1;
        }
    }

    #[inline]
    pub fn push_slice(&mut self, t: &[T]) -> Result<(), MicroVecError> {
        let len = self.0 as usize;
        if len + t.len() >= N {
            return Err(MicroVecError)
        } else {
            self.1[len..len+t.len()].copy_from_slice(t);
            self.0 += t.len() as u8;
            Ok(())
        }
    }
}

impl<T : Clone + Copy, const N : usize> core::ops::Deref for MicroVec<T, N> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[test]
fn test_microvec() {
    let mut mv = MicroVec::<_, 32>::new();
    mv.push(1_u8).unwrap();
    assert_eq!(mv.as_slice(), &[1]);
}
