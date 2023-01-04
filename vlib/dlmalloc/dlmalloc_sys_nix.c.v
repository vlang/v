module dlmalloc

$if !freestanding {
	#include <sys/mman.h>
	#include <unistd.h>
}
fn C.munmap(ptr voidptr, size usize) int
fn C.mremap(ptr voidptr, old usize, new usize, flags usize) voidptr
fn C.mmap(base voidptr, len usize, prot int, flags int, fd int, offset i64) voidptr

pub enum Mm_prot {
	prot_read = 0x1
	prot_write = 0x2
	prot_exec = 0x4
	prot_none = 0x0
	prot_growsdown = 0x01000000
	prot_growsup = 0x02000000
}

pub enum Map_flags {
	map_shared = 0x01
	map_private = 0x02
	map_shared_validate = 0x03
	map_type = 0x0f
	map_fixed = 0x10
	map_file = 0x00
	map_anonymous = 0x20
	map_huge_shift = 26
	map_huge_mask = 0x3f
}

enum MemProt {
	prot_read = 0x1
	prot_write = 0x2
	prot_exec = 0x4
	prot_none = 0x0
	prot_growsdown = 0x01000000
	prot_growsup = 0x02000000
}

enum MapFlags {
	map_shared = 0x01
	map_private = 0x02
	map_shared_validate = 0x03
	map_type = 0x0f
	map_fixed = 0x10
	map_file = 0x00
	map_anonymous = 0x20
	map_huge_shift = 26
	map_huge_mask = 0x3f
}

fn system_alloc(_ voidptr, size usize) (voidptr, usize, u32) {
	$if !freestanding {
		unsafe {
			mem_prot := MemProt(int(MemProt.prot_read) | int(MemProt.prot_write))
			map_flags := MapFlags(int(MapFlags.map_private) | int(MapFlags.map_anonymous))
			addr := C.mmap(nil, size, int(mem_prot), int(map_flags), -1, 0)

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

fn system_remap(_ voidptr, ptr voidptr, oldsize usize, newsize usize, can_move bool) voidptr {
	return unsafe { nil }
}

fn system_free_part(_ voidptr, ptr voidptr, oldsize usize, newsize usize) bool {
	$if linux && !freestanding {
		unsafe {
			rc := C.mremap(ptr, oldsize, newsize, 0)
			if rc != voidptr(-1) {
				return true
			}
			return C.munmap(voidptr(usize(ptr) + newsize), oldsize - newsize) == 0
		}
	} $else $if macos && !freestanding {
		unsafe {
			return C.munmap(voidptr(usize(ptr) + newsize), oldsize - newsize) == 0
		}
	}
	return false
}

fn system_free(_ voidptr, ptr voidptr, size usize) bool {
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

fn system_page_size(_ voidptr) usize {
	return 4096
}

pub fn get_system_allocator() Allocator {
	return Allocator{
		alloc: system_alloc
		remap: system_remap
		free_part: system_free_part
		free_: system_free
		can_release_part: system_can_release_part
		allocates_zeros: system_allocates_zeros
		page_size: system_page_size
		data: unsafe { nil }
	}
}
