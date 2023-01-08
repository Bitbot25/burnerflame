use crate::{
    modrm::{self, ModRM},
    rex::REX,
    Immediate, Instruction, Memory, OpCode, OpCodeFlags, Register, Register32, Register64,
};

pub trait InstructionWith {
    fn new(opcode: OpCode) -> Instruction;
}

pub trait InstructionWith2<A, B> {
    fn new2(opcode: OpCode, opr0: A, opr1: B) -> Instruction;
}

pub trait InstructionWith1<A> {
    fn new1(opcode: OpCode, opr0: A) -> Instruction;
}

impl InstructionWith for Instruction {
    fn new(opcode: OpCode) -> Instruction {
        assert!(
            opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "Mod R/M is not applicable."
        );
        Instruction {
            opcode,
            modrm: None,
            rex: opcode
                .opcode_register_operand()
                .and_then(|reg| REX::maybe_of(reg)), // SPL registers and such REQUIRE a REX prefix
            sib: None,
            displacement: None,
            immediate: None,
        }
    }
}

impl InstructionWith2<Memory, Register32> for Instruction {
    fn new2(opcode: OpCode, opr0: Memory, opr1: Register32) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "Mod R/M is required for this instruction generator."
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::IndirectDisp32,
                reg: modrm::Reg::Register(opr1.into()),
                rm: modrm::RM::Register(Register::dangle()),
            }),
            rex: REX::maybe_of(opr1.into()),
            sib: Some(opr0.sib),
            displacement: opr0.displacement,
            immediate: None,
        }
    }
}

impl InstructionWith2<Register64, u32> for Instruction {
    fn new2(opcode: OpCode, opr0: Register64, opr1: u32) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "Mod R/M is required for this instruction generator."
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::Direct,
                reg: modrm::Reg::OpCodeExtension(opcode.ext()),
                rm: modrm::RM::Register(opr0.into()),
            }),
            rex: Some(REX {
                operand_size_is_64bit: true,
                modrm_reg_extension: 0,
                sib_index_extension: 0,
                modrm_rm_or_sib_base_extension: Register::from(opr0).ext(),
            }),
            sib: None,
            displacement: None,
            immediate: Some(Immediate::Imm32(opr1)),
        }
    }
}

impl InstructionWith1<u32> for Instruction {
    fn new1(opcode: OpCode, opr0: u32) -> Instruction {
        Instruction {
            opcode,
            modrm: if opcode.flags().intersects(OpCodeFlags::NO_MODRM) {
                None
            } else {
                Some(ModRM {
                    mod_: modrm::AddressingMode::Direct,
                    reg: modrm::Reg::OpCodeExtension(opcode.ext()),
                    rm: modrm::RM::Register(Register::dangle()),
                })
            },
            rex: opcode
                .opcode_register_operand()
                .and_then(|reg| REX::maybe_of(reg)), // SPL registers and such REQUIRE a REX prefix
            sib: None,
            displacement: None,
            immediate: Some(Immediate::Imm32(opr0)),
        }
    }
}