module builtin

// The bare V hooks share libc's allocation domain when C libraries are enabled.
// C-owned pointers can then be passed to V's free/realloc, and vice versa.
#include <stdlib.h>

fn C.posix_memalign(pointer &voidptr, alignment uint, size uint) i32

@[unsafe]
fn __malloc(size uint) voidptr {
	return C.malloc(size)
}

@[unsafe]
fn __calloc(count uint, size uint) voidptr {
	return C.calloc(count, size)
}

@[unsafe]
fn __free(pointer voidptr) {
	C.free(pointer)
}

@[unsafe]
fn __malloc_align(size uint, alignment uint) voidptr {
	if alignment == 0 || alignment & (alignment - 1) != 0 { return unsafe { nil } }
	// V permits alignments 1/2/4. POSIX requires a pointer-sized multiple;
	// malloc already meets all those smaller alignment requirements.
	if alignment <= sizeof(voidptr) { return C.malloc(size) }
	mut pointer := voidptr(unsafe { nil })
	if C.posix_memalign(&pointer, alignment, size) != 0 {
		return unsafe { nil }
	}
	return pointer
}

// memcpy copies n bytes from const_src to dest.
@[unsafe]
pub fn memcpy(dest voidptr, const_src voidptr, n uint) voidptr {
	return C.memcpy(dest, const_src, n)
}

// memset fills n bytes at dest with value.
@[unsafe]
pub fn memset(dest voidptr, value int, n uint) voidptr {
	return C.memset(dest, value, n)
}

@[unsafe]
fn memmove(dest voidptr, const_src voidptr, n uint) voidptr {
	return C.memmove(dest, const_src, n)
}

// bare_print writes directly to standard output through the kernel runtime.
pub fn bare_print(buffer &u8, count u64) {
	$if i386 {
		sys_write(1, buffer, count)
	} $else {
		bf_write(1, buffer, int(count))
	}
}

fn bare_eprint(buffer &u8, count u64) {
	$if i386 {
		sys_write(2, buffer, count)
	} $else {
		bf_write(2, buffer, int(count))
	}
}

@[noreturn]
fn bare_panic(message string) {
	eprintln('V panic: ' + message)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

@[noreturn]
fn __exit(code int) {
	C.exit(code)
}

fn __qsort(base voidptr, count uint, size uint, compare C.qsort_callback_func) {
	C.qsort(base, count, size, compare)
}
