module builtin

import dlmalloc

// Corresponding intrinsic to wasm’s `memory.grow` instruction
//
// This function, when called, will attempt to grow the default linear memory by the specified delta of pages.
// The current WebAssembly page size is 65536 bytes (64 KB). If memory is successfully grown then the previous size of memory, in pages, is returned.
// If memory cannot be grown then -1 is returned.
//
// The argument mem is the numerical index of which memory to return the size of. Note that currently the WebAssembly specification only supports one memory,
// so it is required that zero is passed in. The argument is present to be forward-compatible with future WebAssembly revisions.
// If a nonzero argument is passed to this function it will currently unconditionally abort
fn C.__builtin_wasm_memory_grow(mem u32, delta usize) usize

/// Corresponding intrinsic to wasm's `memory.size` instruction
///
/// This function, when called, will return the current memory size in units of
/// pages. The current WebAssembly page size is 65536 bytes (64 KB).
fn C.__builtin_wasm_memory_size(mem u32) usize

const page_size = 65536

fn system_alloc(_ voidptr, size usize) (voidptr, usize, u32) {
	pages := size / page_size
	prev := C.__builtin_wasm_memory_grow(0, pages)
	if prev == -1 {
		return voidptr(0), 0, 0
	}
	return voidptr(prev * page_size), pages * page_size, 0
}

fn system_remap(_ voidptr, _ voidptr, _ usize, _ usize, _ bool) voidptr {
	return voidptr(0)
}

fn system_free_part(_ voidptr, _ voidptr, _ usize, _ usize) bool {
	return false
}

fn system_free(_ voidptr, _ voidptr, _ usize) bool {
	return false
}

fn system_allocates_zeros(_ voidptr) bool {
	return false
}

fn system_page_size(_ voidptr) usize {
	return page_size
}

fn system_can_release_part(_ voidptr, _ u32) bool {
	return false
}

fn get_wasm_allocator() dlmalloc.Allocator {
	return dlmalloc.Allocator{
		alloc: system_alloc
		remap: system_remap
		free_part: system_free_part
		free_: system_free
		can_release_part: system_can_release_part
		allocates_zeros: system_allocates_zeros
		page_size: system_page_size
		data: voidptr(0)
	}
}
