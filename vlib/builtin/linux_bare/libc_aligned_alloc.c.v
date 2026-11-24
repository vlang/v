module builtin

$if !i386 {
	#flag @DIR/libc_aligned_alloc.c
	#include "@DIR/libc_aligned_alloc.h"

	fn C.v_freestanding_aligned_alloc(alignment uint, size uint) voidptr

	@[export: 'v_freestanding_aligned_alloc']
	fn libc_aligned_alloc(alignment uint, size uint) voidptr {
		return unsafe { __malloc_align(size, alignment) }
	}
}
