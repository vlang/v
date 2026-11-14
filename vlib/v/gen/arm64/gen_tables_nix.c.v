module arm64

#include <sys/mman.h>

// zeroed_table_alloc maps `size` bytes of anonymous memory, which the kernel
// zeroes on first touch. Unlike calloc it never clears the whole block up front.
fn zeroed_table_alloc(size int) voidptr {
	data := unsafe {
		C.mmap(nil, uint(size), C.PROT_READ | C.PROT_WRITE, C.MAP_PRIVATE | C.MAP_ANON, -1, 0)
	}
	if data == voidptr(C.MAP_FAILED) {
		panic('arm64: could not map ${size} bytes for code generation tables')
	}
	return data
}

// free_zeroed_table releases a table of `size` bytes from zeroed_table_alloc.
fn free_zeroed_table(data voidptr, size int) {
	if size > 0 {
		unsafe {
			C.munmap(data, uint(size))
		}
	}
}
