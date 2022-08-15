module builtin

import dlmalloc

fn mm_alloc(size u64) (&byte, Errno) {
	// BEGIN CONSTS
	// the constants need to be here, since the initialization of other constants,
	// which happen before these ones would, require malloc
	mem_prot := MemProt(int(MemProt.prot_read) | int(MemProt.prot_write))
	map_flags := MapFlags(int(MapFlags.map_private) | int(MapFlags.map_anonymous))
	// END CONSTS

	a, e := sys_mmap(&u8(0), size + sizeof(u64), mem_prot, map_flags, -1, 0)
	if e == .enoerror {
		unsafe {
			mut ap := &u64(a)
			*ap = size
			x2 := &u8(a + sizeof(u64))
			return x2, e
		}
	}
	return &u8(0), e
}

fn mm_free(addr &u8) Errno {
	unsafe {
		ap := &u64(addr - sizeof(u64))
		size := *ap
		return sys_munmap(addr - sizeof(u64), size + sizeof(u64))
	}
}

fn system_alloc(_ voidptr, size usize) (voidptr, usize, u32) {
	// BEGIN CONSTS
	// the constants need to be here, since the initialization of other constants,
	// which happen before these ones would, require malloc
	mem_prot := MemProt(int(MemProt.prot_read) | int(MemProt.prot_write))
	map_flags := MapFlags(int(MapFlags.map_private) | int(MapFlags.map_anonymous))
	// END CONSTS

	a, e := sys_mmap(&u8(0), u64(size), mem_prot, map_flags, -1, 0)

	if e == .enoerror {
		return a, size, 0
	}
	return unsafe { nil }, 0, 0
}

fn system_remap(_ voidptr, ptr voidptr, oldsize usize, newsize usize, can_move bool) voidptr {
	return unsafe { nil }
}

fn system_free_part(_ voidptr, ptr voidptr, oldsize usize, newsize usize) bool {
	_, e := sys_mremap(ptr, u64(oldsize), u64(newsize), 0)
	if e == .enoerror {
		return true
	}
	e2 := sys_munmap(voidptr(usize(ptr) + newsize), u64(oldsize - newsize))

	return e2 == .enoerror
}

fn system_free(_ voidptr, ptr voidptr, size usize) bool {
	unsafe {
		return sys_munmap(ptr, u64(size)) == .enoerror
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

fn get_linux_allocator() dlmalloc.Allocator {
	return dlmalloc.Allocator{
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
