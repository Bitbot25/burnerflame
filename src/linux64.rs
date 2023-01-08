#![allow(dead_code)]
use std::arch::asm;

pub const PROT_READ: i64 = 0x1;
pub const PROT_WRITE: i64 = 0x2;
pub const PROT_EXEC: i64 = 0x4;
pub const PROT_NONE: i64 = 0x0;
pub const PROT_GROWSDOWN: i64 = 0x01000000;
pub const PROT_GROWSUP: i64 = 0x02000000;

pub const MAP_SHARED: i64 = 0x01;
pub const MAP_PRIVATE: i64 = 0x02;
pub const MAP_TYPE: i64 = 0x0f;
pub const MAP_FIXED: i64 = 0x10;
pub const MAP_FILE: i64 = 0;
pub const MAP_ANONYMOUS: i64 = 0x20;
pub const MAP_32BIT: i64 = 0x40;

pub const SYS_MMAP: i64 = 9;
pub const SYS_MUNMAP: i64 = 11;

pub unsafe fn mmap_raw(
    addr: *mut u8,
    length: usize,
    prot: i64,
    flags: i64,
    fd: i64,
    offset: usize,
) -> *mut u8 {
    let mut result: *mut u8;
    asm!(
        "syscall",
        in("rax") SYS_MMAP,
        in("rdi") addr,
        in("rsi") length,
        in("rdx") prot,
        in("r10") flags,
        in("r8") fd,
        in("r9") offset,
        lateout("rax") result,
        out("rcx") _,
        out("r11") _,
    );
    result
}

pub unsafe fn munmap_raw(addr: *mut u8, length: usize) {
    asm!(
        "syscall",
        in("rax") SYS_MUNMAP,
        in("rdi") addr,
        in("rsi") length,
        out("rcx") _,
        out("r11") _,
    );
}

pub fn round_to_pow2(mut n: usize) -> usize {
    n -= 1;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n |= n >> 32;
    n + 1
}

#[derive(Debug)]
pub struct MMapHandle {
    addr: *mut u8,
    length: usize,
}

impl MMapHandle {
    pub fn executable(slice: &[u8]) -> MMapHandle {
        let page_sz = 4096;
        let segments = round_to_pow2((slice.len() + page_sz - 1) / page_sz);
        let length = segments * page_sz;
        let addr = unsafe {
            mmap_raw(
                std::ptr::null_mut(),
                length,
                PROT_WRITE | PROT_READ | PROT_EXEC,
                MAP_ANONYMOUS | MAP_PRIVATE,
                -1,
                0,
            )
        };
        unsafe { std::ptr::copy_nonoverlapping(slice.as_ptr(), addr, slice.len()) }
        MMapHandle { addr, length }
    }

    pub fn raw(&self) -> *mut u8 {
        self.addr
    }
}

impl std::ops::Drop for MMapHandle {
    fn drop(&mut self) {
        unsafe { munmap_raw(self.addr, self.length) }
    }
}
