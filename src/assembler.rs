use crate::{encode::Encode, Instruction, OpCode, Register16, Register32, Register64, Register8};

#[cfg(feature = "runtime_type_check")]
use crate::Register;

pub struct Assembler {
    buf: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Assembler { buf: Vec::new() }
    }

    pub fn buf(&self) -> &Vec<u8> {
        &self.buf
    }

    pub fn into_buf(self) -> Vec<u8> {
        self.buf
    }
}

impl AssmMov<Register64, Register64> for Assembler {
    fn mov(&mut self, dest: Register64, val: Register64) {
        Instruction::new2(OpCode::Mov_r64m64_r64, dest, val).encode(&mut self.buf);
    }
}

impl AssmMov<Register32, Register32> for Assembler {
    fn mov(&mut self, dest: Register32, val: Register32) {
        Instruction::new2(OpCode::Mov_r32m32_r32, dest, val).encode(&mut self.buf);
    }
}

impl AssmMov<Register16, Register16> for Assembler {
    fn mov(&mut self, dest: Register16, val: Register16) {
        Instruction::new2(OpCode::Mov_r16m16_r16, dest, val).encode(&mut self.buf);
    }
}

impl AssmMov<Register8, Register8> for Assembler {
    fn mov(&mut self, dest: Register8, val: Register8) {
        Instruction::new2(OpCode::Mov_r8m8_r8, dest, val).encode(&mut self.buf);
    }
}

#[cfg(feature = "runtime_type_check")]
macro_rules! assm_mov_try_with {
    ($self:expr,$dest:expr,$val:expr,$destty:ty,$valty:ty) => {
        <Self as AssmMov<$destty, $valty>>::mov($self, <$destty>::new($dest), <$valty>::new($val))
    };
}

#[cfg(feature = "runtime_type_check")]
impl AssmMov<Register, Register> for Assembler {
    fn mov(&mut self, dest: Register, val: Register) {
        fn incompatible() {
            panic!("incompatible register types, use movzx instead");
        }
        // TODO: Fix this mess
        if dest.is_64bit() {
            if !val.is_64bit() {
                incompatible();
            }
            assm_mov_try_with!(self, dest, val, Register64, Register64)
        } else if dest.is_32bit() {
            if !val.is_32bit() {
                incompatible();
            }
            assm_mov_try_with!(self, dest, val, Register32, Register32)
        } else if dest.is_16bit() {
            if !val.is_16bit() {
                incompatible();
            }
            assm_mov_try_with!(self, dest, val, Register16, Register16);
        } else if dest.is_8bit() {
            if !val.is_8bit() {
                incompatible();
            }
            assm_mov_try_with!(self, dest, val, Register8, Register8)
        }
    }
}

impl AssmMov<Register64, u64> for Assembler {
    fn mov(&mut self, dest: Register64, val: u64) {
        Instruction::new1(OpCode::Mov_r64_imm64(dest), val).encode(&mut self.buf)
    }
}

#[cfg(feature = "runtime_type_check")]
impl AssmMov<Register, u64> for Assembler {
    fn mov(&mut self, dest: Register, val: u64) {
        if !dest.is_64bit() {
            panic!("cannot move 64-bit value into non 64-bit register, use movzx instead.");
        }
        <Self as AssmMov<Register64, u64>>::mov(self, Register64::new(dest), val)
    }
}

impl AssmMov<Register32, u32> for Assembler {
    fn mov(&mut self, dest: Register32, val: u32) {
        Instruction::new1(OpCode::Mov_r32_imm32(dest), val).encode(&mut self.buf)
    }
}

#[cfg(feature = "runtime_type_check")]
impl AssmMov<Register, u32> for Assembler {
    fn mov(&mut self, dest: Register, val: u32) {
        if !dest.is_32bit() {
            panic!("cannot mov 32-bit into non 32-bit register, use movzx instead.")
        }

        <Self as AssmMov<Register32, u32>>::mov(self, Register32::new(dest), val)
    }
}

impl AssmRet for Assembler {
    fn ret(&mut self) {
        Instruction::new(OpCode::Retn).encode(&mut self.buf)
    }
}

pub trait AssmMov<A, B> {
    fn mov(&mut self, dest: A, val: B);
}

pub trait AssmRet {
    fn ret(&mut self);
}
