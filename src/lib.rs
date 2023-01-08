use bitflags::bitflags;
use enum_map::{enum_map, Enum, EnumMap};

pub mod encode;
pub mod linux64;
mod make_ins;
mod reg;

bitflags! {
    pub struct RegisterFlags: u8 {
        const IS_64BIT = 0b00000001;
        const IS_32BIT = 0b00000010;
        const IS_16BIT = 0b00000100;
        const IS_8BIT  = 0b00001000;
    }
}

struct RegisterData {
    main_byte: u8,
    extension: Option<RegisterExtension>,
    flags: RegisterFlags,
}

fn register_data_map() -> EnumMap<Register, RegisterData> {
    // TODO: Change into lazy constant
    enum_map! {
        Register::AL => RegisterData { main_byte: 0, extension: None, flags: RegisterFlags::IS_8BIT },
        Register::CL => RegisterData { main_byte: 1, extension: None, flags: RegisterFlags::IS_8BIT },
        Register::DL => RegisterData { main_byte: 2, extension: None, flags: RegisterFlags::IS_8BIT },
        Register::BL => RegisterData { main_byte: 3, extension: None, flags: RegisterFlags::IS_8BIT },

        Register::AX => RegisterData { main_byte: 0, extension: None, flags: RegisterFlags::IS_16BIT },
        Register::CX => RegisterData { main_byte: 1, extension: None, flags: RegisterFlags::IS_16BIT },
        Register::DX => RegisterData { main_byte: 2, extension: None, flags: RegisterFlags::IS_16BIT },
        Register::BX => RegisterData { main_byte: 3, extension: None, flags: RegisterFlags::IS_16BIT },

        Register::EAX => RegisterData { main_byte: 0, extension: None, flags: RegisterFlags::IS_32BIT },
        Register::ECX => RegisterData { main_byte: 1, extension: None, flags: RegisterFlags::IS_32BIT },
        Register::EDX => RegisterData { main_byte: 2, extension: None, flags: RegisterFlags::IS_32BIT },
        Register::EBX => RegisterData { main_byte: 3, extension: None, flags: RegisterFlags::IS_32BIT },

        Register::RAX => RegisterData { main_byte: 0, extension: None, flags: RegisterFlags::IS_64BIT },
        Register::RCX => RegisterData { main_byte: 1, extension: None, flags: RegisterFlags::IS_64BIT },
        Register::RDX => RegisterData { main_byte: 2, extension: None, flags: RegisterFlags::IS_64BIT },
        Register::RBX => RegisterData { main_byte: 3, extension: None, flags: RegisterFlags::IS_64BIT },
    }
}

#[derive(Debug, Clone, Copy, Enum)]
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
    pub fn encoding(&self) -> (u8, Option<RegisterExtension>) {
        let RegisterData {
            main_byte,
            extension,
            ..
        } = register_data_map()[*self];
        (main_byte, extension)
    }

    pub fn is_64bit(&self) -> bool {
        register_data_map()[*self]
            .flags
            .intersects(RegisterFlags::IS_64BIT)
    }

    pub fn is_32bit(&self) -> bool {
        register_data_map()[*self]
            .flags
            .intersects(RegisterFlags::IS_32BIT)
    }

    pub fn is_16bit(&self) -> bool {
        register_data_map()[*self]
            .flags
            .intersects(RegisterFlags::IS_16BIT)
    }

    pub fn is_8bit(&self) -> bool {
        register_data_map()[*self]
            .flags
            .intersects(RegisterFlags::IS_8BIT)
    }

    pub fn ext(&self) -> Option<RegisterExtension> {
        let (_enc, ext) = self.encoding();
        ext
    }

    pub fn dangle() -> Self {
        Register::AL
    }
}

#[derive(Debug)]
pub struct Memory {
    pub sib: Sib,
    pub displacement: Option<DisplacementByte>,
}

#[derive(Clone, Copy)]
pub struct Register8(Register);
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
            panic!("Cannot wrap non 64-bit register in Register64")
        }
    }
}

impl Register32 {
    pub fn new(reg: Register) -> Self {
        if reg.is_32bit() {
            Register32(reg)
        } else {
            panic!("Cannot wrap non 32-bit register in Register32")
        }
    }
}

impl Register16 {
    pub fn new(reg: Register) -> Self {
        if reg.is_16bit() {
            Register16(reg)
        } else {
            panic!("Cannot wrap non 16-bit register in Register16")
        }
    }
}

impl Register8 {
    pub fn new(reg: Register) -> Self {
        if reg.is_8bit() {
            Register8(reg)
        } else {
            panic!("Cannot wrap non 8-bit register in Register8")
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

use rex::Rex;
mod rex {
    use crate::{Register, RegisterExtension, SIBIndexExtension, SingleBitExtension};

    pub struct Rex {
        pub operand_size_is_64bit: bool,
        pub modrm_reg_extension: RegisterExtension,
        pub sib_index_extension: SIBIndexExtension,
        pub modrm_rm_or_sib_base_extension: SingleBitExtension,
    }

    impl Rex {
        pub fn maybe_of(_register: Register) -> Option<Self> {
            // FIXME: Support for other registers
            None
        }
    }
}

use sib::Sib;
mod sib {
    use super::{u2, Register};

    pub type Scale = u2;
    pub type Index = Register;
    pub type Base = Register;

    #[derive(Debug)]
    pub struct Sib {
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
    pub rex: Option<Rex>,
    pub sib: Option<Sib>,
    pub displacement: Option<DisplacementByte>,
    pub opcode: OpCode,
    pub immediate: Option<Immediate>,
}

#[cfg(test)]
mod test {
    use crate as burnerflame;
    use burnerflame::encode::Encode;
    use burnerflame::linux64::MMapHandle;
    use burnerflame::{Instruction, OpCode};
    use std::mem;

    #[test]
    fn u64_function_returns() {
        type FuncType = unsafe extern "C" fn() -> u32;

        let mov = Instruction::new1(OpCode::Mov_eax_imm32, 32u32);
        let ret = Instruction::new(OpCode::Retn);

        let mut code = Vec::new();
        mov.encode(&mut code);
        ret.encode(&mut code);

        let executable_handle = MMapHandle::executable(code.as_slice());
        unsafe {
            let func = mem::transmute::<*const u8, FuncType>(executable_handle.raw());
            assert_eq!(func(), 32u32);
        }
    }
}
