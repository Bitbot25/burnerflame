# Burnerflame 🔥
AMD64 (x86-64) assembler written in rust.

## Examples
Assembling a function that returns the meaning of the universe
```rust
use std::mem;
use burnerflame::encode::Encode;
use burnerflame::linux64::MMapHandle;
use burnerflame::{Instruction, OpCode};

type FuncType = unsafe extern "C" fn() -> u32;
 
let mov = Instruction::new1(OpCode::Mov_eax_imm32, 42u32);
let ret = Instruction::new(OpCode::Retn);

let mut code = Vec::new();
mov.encode(&mut code);
ret.encode(&mut code);

let executable_handle = MMapHandle::executable(code.as_slice());
unsafe {
    let meaning_of_universe = mem::transmute::<*const u8, FuncType>(executable_handle.raw());
    assert_eq!(meaning_of_universe(), 42u32);
}

```
