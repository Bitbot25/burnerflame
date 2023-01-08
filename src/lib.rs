use bitflags::bitflags;
use enum_map::{enum_map, Enum};

mod encode;
mod linux64;
mod make_ins;
mod reg;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Register {
    AL,
    CL,
    DL,
    BL,

    AX,
    CX,
    DX,
    BX,

    EAX,
    ECX,
    EDX,
    EBX,

    RAX,
    RCX,
    RDX,
    RBX,
}

impl Register {
    pub fn encoding(&self) -> (u8, RegisterExtension) {
        // TODO: enum_map
        match self {
            Self::AL | Self::AX | Self::EAX | Self::RAX => (0, RegisterExtension::min_value()),
            Self::CL | Self::CX | Self::ECX | Self::RCX => (1, RegisterExtension::min_value()),
            Self::DL | Self::DX | Self::EDX | Self::RDX => (2, RegisterExtension::min_value()),
            Self::BL | Self::BX | Self::EBX | Self::RBX => (3, RegisterExtension::min_value()),
        }
    }

    pub fn is_64bit(&self) -> bool {
        match self {
            Self::AL
            | Self::AX
            | Self::CL
            | Self::CX
            | Self::DL
            | Self::DX
            | Self::BL
            | Self::BX
            | Self::EAX
            | Self::ECX
            | Self::EDX
            | Self::EBX => false,
            Self::RAX | Self::RCX | Self::RDX | Self::RBX => true,
        }
    }

    pub fn ext(&self) -> RegisterExtension {
        let (_enc, ext) = self.encoding();
        ext
    }

    pub fn dangle() -> Self {
        Register::AL
    }
}

#[derive(Debug)]
pub struct Memory {
    pub sib: SIB,
    pub displacement: Option<DisplacementByte>,
}

#[derive(Clone, Copy)]
pub struct Register16(Register);
#[derive(Clone, Copy)]
pub struct Register32(Register);
#[derive(Clone, Copy)]
pub struct Register64(Register);

impl Register64 {
    pub fn new(reg: Register) -> Self {
        if reg.is_64bit() {
            Register64(reg)
        } else {
            panic!("Cannot wrap non-64bit register in Register64")
        }
    }
}

#[allow(non_camel_case_types)]
type u2 = u8;
#[allow(non_camel_case_types)]
type u3 = u8;
#[allow(non_camel_case_types)]
type u1 = u8;

type OpCodeExtension = ThreeBitExtension;
type RegisterExtension = SingleBitExtension;
type SIBIndexExtension = SingleBitExtension;
type ThreeBitExtension = u3;
type SingleBitExtension = u1;

use modrm::ModRM;
mod modrm {
    use crate::OpCodeExtension;

    use super::Register;

    #[repr(u8)]
    #[rustfmt::skip]
    #[derive(Clone, Copy, Debug)]
    pub enum AddressingMode {
        Direct          = 0b11000000,
        IndirectGeneric = 0b00000000,
        IndirectDisp8   = 0b01000000,
        IndirectDisp32  = 0b10000000,
    }

    pub enum Reg {
        OpCodeExtension(OpCodeExtension),
        Register(Register),
    }

    pub enum RM {
        Register(Register),
    }

    pub struct ModRM {
        pub mod_: AddressingMode,
        pub reg: Reg,
        pub rm: RM,
    }
}

use rex::REX;
mod rex {
    use crate::{Register, RegisterExtension, SIBIndexExtension, SingleBitExtension};

    pub struct REX {
        pub operand_size_is_64bit: bool,
        pub modrm_reg_extension: RegisterExtension,
        pub sib_index_extension: SIBIndexExtension,
        pub modrm_rm_or_sib_base_extension: SingleBitExtension,
    }

    impl REX {
        pub fn maybe_of(_register: Register) -> Option<Self> {
            // FIXME: Support for other registers
            None
        }
    }
}

use sib::SIB;
mod sib {
    use super::{u2, Register};

    pub type Scale = u2;
    pub type Index = Register;
    pub type Base = Register;

    #[derive(Debug)]
    pub struct SIB {
        pub scale: Scale,
        pub index: Index,
        pub base: Base,
    }
}

#[derive(Clone, Copy, Debug, Enum)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum OpCode {
    Mov_r8m8_imm8,
    Mov_r16m16_imm16,
    Mov_r32m32_imm32,
    Mov_r64m64_simm32,

    Mov_rax_imm64,
    Mov_eax_imm32,
    Mov_ax_imm16,
    Mov_al_imm8,

    Retn,
    Retn_imm16,
}

impl OpCode {
    pub fn encoding(&self) -> (u8, OpCodeExtension) {
        match self {
            Self::Mov_r8m8_imm8 => (0xc6, 0),
            Self::Mov_r16m16_imm16 => (0xc7, 0),
            Self::Mov_r32m32_imm32 => (0xc7, 0),
            Self::Mov_r64m64_simm32 => (0xc7, 0),

            Self::Mov_rax_imm64 | Self::Mov_eax_imm32 | Self::Mov_ax_imm16 => (0xb8, 0),
            Self::Mov_al_imm8 => (0xb0, 0),

            Self::Retn => (0xc3, 0),
            Self::Retn_imm16 => (0xc2, 0),
        }
    }

    pub fn opcode_register_operand(&self) -> Option<Register> {
        match self {
            Self::Mov_rax_imm64 => Some(Register::RAX),
            Self::Mov_eax_imm32 => Some(Register::EAX),
            Self::Mov_ax_imm16 => Some(Register::AX),
            Self::Mov_al_imm8 => Some(Register::AL),

            Self::Mov_r8m8_imm8
            | Self::Mov_r16m16_imm16
            | Self::Mov_r32m32_imm32
            | Self::Mov_r64m64_simm32
            | Self::Retn
            | Self::Retn_imm16 => None,
        }
    }

    pub fn ext(&self) -> OpCodeExtension {
        let (_enc, ext) = self.encoding();
        ext
    }

    pub fn flags(&self) -> OpCodeFlags {
        let opcode_flags = enum_map! {
            OpCode::Mov_r8m8_imm8 => OpCodeFlags::empty(),
            OpCode::Mov_r16m16_imm16 => OpCodeFlags::empty(),
            OpCode::Mov_r32m32_imm32 => OpCodeFlags::empty(),
            OpCode::Mov_r64m64_simm32 => OpCodeFlags::empty(),
            OpCode::Mov_rax_imm64 => OpCodeFlags::NO_MODRM,
            OpCode::Mov_eax_imm32 => OpCodeFlags::NO_MODRM,
            OpCode::Mov_ax_imm16 => OpCodeFlags::NO_MODRM,
            OpCode::Mov_al_imm8 => OpCodeFlags::NO_MODRM,
            OpCode::Retn => OpCodeFlags::NO_MODRM,
            OpCode::Retn_imm16 => OpCodeFlags::NO_MODRM,
        };
        opcode_flags[*self]
    }
}

bitflags! {
    pub struct OpCodeFlags: u8 {
        const NO_MODRM = 0b00000001;
    }
}

#[derive(Debug)]
pub enum DisplacementByte {
    SB(u8),
    DB(u16),
    SI(u32),
}

pub enum Immediate {
    Imm8(u8),
    Imm16(u16),
    Imm32(u32),
    Imm64(u64),
}

pub struct Instruction {
    pub modrm: Option<ModRM>,
    pub rex: Option<REX>,
    pub sib: Option<SIB>,
    pub displacement: Option<DisplacementByte>,
    pub opcode: OpCode,
    pub immediate: Option<Immediate>,
}

#[cfg(test)]
mod test {
    use crate::{
        encode::Encode,
        linux64::*,
        make_ins::{InstructionWith, InstructionWith1},
        Instruction, OpCode,
    };
    #[test]
    fn u64_function_returns() {
        let mov = Instruction::new1(OpCode::Mov_eax_imm32, 32u32);
        let ret = Instruction::new(OpCode::Retn);
        let instructions = vec![mov, ret];

        let mut code = Vec::new();
        for instruction in instructions {
            instruction.encode(&mut code);
        }

        let exec = MMapHandle::executable(code.as_slice());
        type FuncType = unsafe extern "C" fn() -> u32;
        let func = unsafe { std::mem::transmute::<*const u8, FuncType>(exec.raw()) };
        assert_eq!(unsafe { func() }, 32);
    }
}
