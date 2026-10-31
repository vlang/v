module dlmalloc

fn system_alloc(_ voidptr, size uint) (voidptr, uint, u32) {
	return unsafe { nil }, 0, 0
}

fn system_remap(_ voidptr, ptr voidptr, oldsize uint, newsize uint, can_move bool) voidptr {
	return unsafe { nil }
}

fn system_free_part(_ voidptr, ptr voidptr, oldsize uint, newsize uint) bool {
	return false
}

fn system_free(_ voidptr, ptr voidptr, size uint) bool {
	return false
}

fn system_can_release_part(_ voidptr, _ u32) bool {
	return false
}

fn system_allocates_zeros(_ voidptr) bool {
	return false
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
