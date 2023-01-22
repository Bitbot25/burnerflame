use crate::{
    make_ins::*,
    modrm::{self, ModRM},
    rex::Rex,
    Immediate, Instruction, Memory, OpCode, OpCodeFlags, Register, Register16, Register32,
    Register64, Register8,
};

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
                .and_then(Rex::activate_if_special), // SPL registers and such REQUIRE a REX prefix
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
            rex: Rex::activate_if_special(opr1.into()),
            sib: Some(opr0.sib),
            displacement: opr0.displacement,
            immediate: None,
        }
    }
}

impl InstructionWith1<u64> for Instruction {
    fn new1(opcode: OpCode, opr0: u64) -> Instruction {
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
            rex: Some(Rex {
                operand_size_is_64bit: true,
                modrm_reg_extension: 0,
                sib_index_extension: 0,
                modrm_rm_or_sib_base_extension: 0,
            }),
            sib: None,
            displacement: None,
            immediate: Some(Immediate::Imm64(opr0)),
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
            rex: Some(Rex {
                operand_size_is_64bit: true,
                modrm_reg_extension: 0,
                sib_index_extension: 0,
                modrm_rm_or_sib_base_extension: Register::from(opr0).ext().unwrap_or(0),
            }),
            sib: None,
            displacement: None,
            immediate: Some(Immediate::Imm32(opr1)),
        }
    }
}

impl InstructionWith2<Register32, u32> for Instruction {
    fn new2(opcode: OpCode, opr0: Register32, opr1: u32) -> Instruction {
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
            rex: Rex::activate_if_special(opr0.into()),
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
                .and_then(Rex::activate_if_special), // SPL registers and such REQUIRE a REX prefix
            sib: None,
            displacement: None,
            immediate: Some(Immediate::Imm32(opr0)),
        }
    }
}

impl InstructionWith2<Register64, Register64> for Instruction {
    fn new2(opcode: OpCode, opr0: Register64, opr1: Register64) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "modrm is required for this instruction generator"
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::Direct,
                reg: modrm::Reg::Register(*opr0),
                rm: modrm::RM::Register(*opr1),
            }),
            rex: Some(Rex {
                operand_size_is_64bit: true,
                modrm_reg_extension: opr0.ext().unwrap_or(0),
                sib_index_extension: 0,
                modrm_rm_or_sib_base_extension: opr1.ext().unwrap_or(0),
            }),
            sib: None,
            displacement: None,
            immediate: None,
        }
    }
}

impl InstructionWith2<Register32, Register32> for Instruction {
    fn new2(opcode: OpCode, opr0: Register32, opr1: Register32) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "modrm is required for this instruction generator"
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::Direct,
                reg: modrm::Reg::Register(*opr0),
                rm: modrm::RM::Register(*opr1),
            }),
            rex: Some(Rex {
                operand_size_is_64bit: false,
                modrm_reg_extension: opr0.ext().unwrap_or(0),
                sib_index_extension: 0,
                modrm_rm_or_sib_base_extension: opr1.ext().unwrap_or(0),
            }),
            sib: None,
            displacement: None,
            immediate: None,
        }
    }
}
impl InstructionWith2<Register16, Register16> for Instruction {
    fn new2(opcode: OpCode, opr0: Register16, opr1: Register16) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "modrm is required for this instruction generator"
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::Direct,
                reg: modrm::Reg::Register(*opr0),
                rm: modrm::RM::Register(*opr1),
            }),
            rex: Rex::activate_if_special2(*opr0, *opr1),
            sib: None,
            displacement: None,
            immediate: None,
        }
    }
}

impl InstructionWith2<Register8, Register8> for Instruction {
    fn new2(opcode: OpCode, opr0: Register8, opr1: Register8) -> Instruction {
        assert!(
            !opcode.flags().intersects(OpCodeFlags::NO_MODRM),
            "modrm is required for this instruction generator"
        );
        Instruction {
            opcode,
            modrm: Some(ModRM {
                mod_: modrm::AddressingMode::Direct,
                reg: modrm::Reg::Register(*opr0),
                rm: modrm::RM::Register(*opr1),
            }),
            rex: Rex::activate_if_special2(*opr0, *opr1),
            sib: None,
            displacement: None,
            immediate: None,
        }
    }
}
