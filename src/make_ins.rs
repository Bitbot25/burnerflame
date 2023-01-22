use crate::{Instruction, OpCode};

mod private_traits {
    use super::*;

    pub trait InstructionWith {
        fn new(opcode: OpCode) -> Self;
    }

    pub trait InstructionWith2<A, B> {
        fn new2(opcode: OpCode, opr0: A, opr1: B) -> Instruction;
    }

    pub trait InstructionWith1<A> {
        fn new1(opcode: OpCode, opr0: A) -> Instruction;
    }
}

pub(crate) use private_traits::*;

impl Instruction {
    pub fn new(opcode: OpCode) -> Self {
        <Self as InstructionWith>::new(opcode)
    }

    pub fn new1<A>(opcode: OpCode, opr0: A) -> Self
    where
        Self: InstructionWith1<A>,
    {
        <Self as InstructionWith1<A>>::new1(opcode, opr0)
    }

    pub fn new2<A, B>(opcode: OpCode, opr0: A, opr1: B) -> Self
    where
        Self: InstructionWith2<A, B>,
    {
        <Self as InstructionWith2<A, B>>::new2(opcode, opr0, opr1)
    }
}
