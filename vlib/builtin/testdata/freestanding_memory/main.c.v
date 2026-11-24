module main

#include "@DIR/probe.h"

fn C.v_test_memchr() i32
fn C.v_test_memchr_boundary(end &u8) i32

@[aligned: 64]
struct AlignedBlock {
mut:
	values [17]u64
}

fn require_memory(condition bool) {
	if !condition { exit(91) }
}

fn main() {
	require_memory(C.v_test_memchr() == 0)
	$if vlibc ? {
		// The second page is inaccessible: searches must stop at the first match
		// and must not inspect any byte beyond a complete, unsuccessful range.
		address, code := vlibc_sys_mmap(unsafe { nil }, 8192, 3, 0x22, -1, 0)
		require_memory(code == .enoerror)
		end := unsafe { &u8(address) + 4096 }
		require_memory(vlibc_sys_mprotect(end, 4096, 0) == .enoerror)
		require_memory(C.v_test_memchr_boundary(end) == 0)
		require_memory(vlibc_sys_munmap(address, 8192) == .enoerror)
	}
	mut source := [257]u8{}
	for i in 0 .. source.len { source[i] = u8(i) }
	for alignment in [16, 32, 64, 4096] {
		copied := unsafe { &u8(memdup_align(&source[0], source.len, alignment)) }
		require_memory(uint(copied) % uint(alignment) == 0)
		for i in 0 .. source.len { require_memory(unsafe { copied[i] } == source[i]) }
		unsafe { free(copied) }
	}
	// Taking the address of a literal and cloning aligned arrays exercise the
	// generated v3_aligned_memdup route as well as the public primitive above.
	mut block := &AlignedBlock{}
	require_memory(uint(block) % 64 == 0)
	block.values[16] = 731
	require_memory(block.values[16] == 731)
	mut blocks := []AlignedBlock{len: 2}
	blocks[1].values[16] = 975
	copied_blocks := blocks.clone()
	require_memory(uint(copied_blocks.data) % 64 == 0)
	require_memory(copied_blocks[1].values[16] == 975)
	println('freestanding memory passed')
}
