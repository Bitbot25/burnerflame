use crate::{Register, Register32, Register64};

impl From<Register32> for Register {
    fn from(reg32: Register32) -> Self {
        reg32.0
    }
}

impl From<Register64> for Register {
    fn from(reg64: Register64) -> Self {
        reg64.0
    }
}
