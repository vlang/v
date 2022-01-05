module dlmalloc

const (
	global = new(get_system_allocator())
)

[unsafe]
pub fn malloc(size usize) voidptr {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		return glob.malloc(size)
	}
}

[unsafe]
pub fn free(ptr voidptr) {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		glob.free_(ptr)
	}
}
