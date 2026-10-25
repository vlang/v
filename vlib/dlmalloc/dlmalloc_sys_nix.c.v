module dlmalloc

$if !freestanding {
	#include <sys/mman.h>
	#include <unistd.h>
}

fn C.munmap(ptr voidptr, size uint) i32
fn C.mmap(base voidptr, len uint, prot i32, flags i32, fd i32, offset i64) voidptr

// Mm_prot and Map_flags hold the Linux values. `system_alloc` uses the C
// constants of the target platform instead: `MAP_ANONYMOUS` differs between
// systems (0x20 on Linux, 0x1000 on macOS and the BSDs).
pub enum Mm_prot {
	prot_read      = 0x1
	prot_write     = 0x2
	prot_exec      = 0x4
	prot_none      = 0x0
	prot_growsdown = 0x01000000
	prot_growsup   = 0x02000000
}

pub enum Map_flags {
	map_shared          = 0x01
	map_private         = 0x02
	map_shared_validate = 0x03
	map_type            = 0x0f
	map_fixed           = 0x10
	map_file            = 0x00
	map_anonymous       = 0x20
	map_huge_shift      = 26
	map_huge_mask       = 0x3f
}

fn system_alloc(_ voidptr, size uint) (voidptr, uint, u32) {
	$if !freestanding {
		unsafe {
			addr := C.mmap(nil, size, C.PROT_READ | C.PROT_WRITE, C.MAP_PRIVATE | C.MAP_ANONYMOUS,
				-1, 0)

			if addr == voidptr(-1) {
				return nil, 0, 0
			} else {
				return addr, size, 0
			}
		}
	} $else {
		return unsafe { nil }, 0, 0
	}
	return unsafe { nil }, 0, 0
}

fn system_remap(_ voidptr, ptr voidptr, oldsize uint, newsize uint, can_move bool) voidptr {
	return unsafe { nil }
}

fn system_free_part(_ voidptr, ptr voidptr, oldsize uint, newsize uint) bool {
	$if !freestanding {
		// Unmapping the tail shrinks the mapping in place, like `mremap` without
		// MREMAP_MAYMOVE would, but needs no GNU-only declaration.
		unsafe {
			return C.munmap(voidptr(uint(ptr) + newsize), oldsize - newsize) == 0
		}
	}
	return false
}

fn system_free(_ voidptr, ptr voidptr, size uint) bool {
	$if !freestanding {
		unsafe {
			return C.munmap(ptr, size) == 0
		}
	} $else {
		return false
	}
}

fn system_can_release_part(_ voidptr, _ u32) bool {
	return true
}

fn system_allocates_zeros(_ voidptr) bool {
	return true
}

fn system_page_size(_ voidptr) uint {
	return 4096
}

// get_system_allocator returns an allocator that uses the system allocator.
pub fn get_system_allocator() Allocator {
	return Allocator{
		alloc:            system_alloc
		remap:            system_remap
		free_part:        system_free_part
		free_:            system_free
		can_release_part: system_can_release_part
		allocates_zeros:  system_allocates_zeros
		page_size:        system_page_size
		data:             unsafe { nil }
	}
}
