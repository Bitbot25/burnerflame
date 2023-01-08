use crate::{
    modrm::{self, ModRM},
    rex::Rex,
    sib::Sib,
    DisplacementByte, Immediate, Instruction,
};

pub trait Encode {
    fn encode(&self, out: &mut Vec<u8>);
}

impl Encode for Instruction {
    fn encode(&self, out: &mut Vec<u8>) {
        match &self.rex {
            Some(rex) => rex.encode(out),
            None => (),
        }

        let (opcode, _) = self.opcode.encoding();
        out.push(opcode);
        match &self.modrm {
            Some(modrm) => modrm.encode(out),
            None => (),
        };

        match &self.sib {
            Some(sib) => sib.encode(out),
            None => (),
        };

        match &self.displacement {
            Some(displacement) => displacement.encode(out),
            None => (),
        };

        match &self.immediate {
            Some(immediate) => immediate.encode(out),
            None => (),
        };
    }
}

impl Encode for Immediate {
    fn encode(&self, out: &mut Vec<u8>) {
        match self {
            Self::Imm8(imm8) => out.push(*imm8),
            Self::Imm16(imm16) => out.extend(imm16.to_le_bytes()),
            Self::Imm32(imm32) => out.extend(imm32.to_le_bytes()),
            Self::Imm64(imm64) => out.extend(imm64.to_le_bytes()),
        }
    }
}

impl Encode for DisplacementByte {
    fn encode(&self, out: &mut Vec<u8>) {
        match self {
            Self::SB(sb) => out.push(*sb),
            Self::DB(db) => out.extend(db.to_le_bytes()),
            Self::SI(si) => out.extend(si.to_le_bytes()),
        };
    }
}

impl Encode for Sib {
    fn encode(&self, out: &mut Vec<u8>) {
        let (index, _ext) = self.index.encoding();
        let (base, _ext) = self.base.encoding();
        let sib = self.scale | (index << 6) | base;
        eprintln!("SIB: {:08b}", sib);
        out.push(sib);
    }
}

impl Encode for ModRM {
    fn encode(&self, out: &mut Vec<u8>) {
        let (rm, _ext) = match self.rm {
            modrm::RM::Register(rm) => rm,
        }
        .encoding();
        let (reg, _ext) = match self.reg {
            modrm::Reg::OpCodeExtension(_op) => (0, 0),
            modrm::Reg::Register(reg) => reg.encoding(),
        };
        let modrm = self.mod_ as u8 | (reg << 3) | rm;
        out.push(modrm);
    }
}

impl Encode for Rex {
    fn encode(&self, out: &mut Vec<u8>) {
        let mut rex = 0b01000000;
        rex |= (self.operand_size_is_64bit as u8) << 3;
        rex |= self.modrm_reg_extension << 2;
        rex |= self.sib_index_extension << 1;
        rex |= self.modrm_rm_or_sib_base_extension;
        out.push(rex);
    }
}
