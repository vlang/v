module builtin

import dlmalloc

__global global_allocator dlmalloc.Dlmalloc
__global global_allocator_ready bool

// memcpy copies n bytes from const_src to dest.
@[export: 'memcpy']
@[unsafe]
pub fn memcpy(dest voidptr, const_src voidptr, n uint) voidptr {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	unsafe {
		for i in 0 .. int(n) {
			dest_[i] = src_[i]
		}
	}
	return unsafe { dest }
}

// __malloc sets up the allocator on first use: `_vinit` and the constants it
// initializes already allocate, so it cannot wait for any of them.
@[export: 'malloc']
@[unsafe]
fn __malloc(n uint) voidptr {
	bare_heap_lock()
	if !global_allocator_ready {
		init_global_allocator()
	}
	result := unsafe { global_allocator.malloc(n) }
	bare_heap_unlock()
	if result == unsafe { nil } {
		$if linux && !i386 {
			bf_native_errno_cell = 12
		}
	}
	return result
}

// __malloc_align returns a normal bare-heap allocation with the requested
// power-of-two alignment. Invalid requests and unrepresentable sizes return nil.
// dlmalloc owns the aligned block, so __free and realloc accept its base pointer.
@[markused; unsafe]
fn __malloc_align(size uint, alignment uint) voidptr {
	if alignment == 0 || alignment & (alignment - 1) != 0
		|| alignment > uint(max_int) || size > uint(max_int) - alignment {
		return unsafe { nil }
	}
	if alignment <= 2 * sizeof(uint) {
		return unsafe { __malloc(size) }
	}
	bare_heap_lock()
	if !global_allocator_ready {
		init_global_allocator()
	}
	result := unsafe { global_allocator.memalign(alignment, size) }
	bare_heap_unlock()
	if result == unsafe { nil } {
		$if linux && !i386 {
			bf_native_errno_cell = 12
		}
	}
	return result
}

@[export: 'strlen']
@[unsafe]
fn strlen(const_s voidptr) uint {
	s := unsafe { &u8(const_s) }
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {
	}
	return uint(i)
}

@[export: 'realloc']
@[unsafe]
fn realloc(old_area voidptr, new_size uint) voidptr {
	if old_area == 0 {
		return unsafe { __malloc(new_size) }
	}
	if new_size == uint(0) {
		unsafe { __free(old_area) }
		return 0
	}
	bare_heap_lock()
	result := unsafe { global_allocator.realloc(old_area, new_size) }
	bare_heap_unlock()
	if result == unsafe { nil } {
		$if linux && !i386 {
			bf_native_errno_cell = 12
		}
	}
	return result
}

@[export: 'memset']
@[unsafe]
fn memset(s voidptr, c int, n uint) voidptr {
	mut s_ := unsafe { &char(s) }
	for i in 0 .. int(n) {
		unsafe {
			s_[i] = char(c)
		}
	}
	return unsafe { s }
}

// memmove copies backwards when `dest` overlaps the end of the source, so it
// needs no temporary buffer.
@[export: 'memmove']
@[unsafe]
fn memmove(dest voidptr, const_src voidptr, n uint) voidptr {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	if uint(dest_) <= uint(src_) || uint(dest_) >= uint(src_) + n {
		for i in 0 .. int(n) {
			unsafe {
				dest_[i] = src_[i]
			}
		}
	} else {
		for i := int(n) - 1; i >= 0; i-- {
			unsafe {
				dest_[i] = src_[i]
			}
		}
	}
	return unsafe { dest }
}

@[export: 'calloc']
@[unsafe]
fn __calloc(nmemb uint, size uint) voidptr {
	if size != 0 && nmemb > ~uint(0) / size {
		$if linux && !i386 {
			bf_native_errno_cell = 12
		}
		return unsafe { nil }
	}
	new_area := unsafe { __malloc(nmemb * size) }
	if new_area != 0 {
		unsafe { memset(new_area, 0, nmemb * size) }
	}
	return new_area
}

fn getchar() int {
	x := u8(0)
	sys_read(0, &x, 1)
	return int(x)
}

@[export: 'memcmp']
fn memcmp(const_a voidptr, const_b voidptr, n uint) int {
	a_ := unsafe { &u8(const_a) }
	b_ := unsafe { &u8(const_b) }
	for i in 0 .. int(n) {
		if unsafe { a_[i] != b_[i] } {
			unsafe {
				return a_[i] - b_[i]
			}
		}
	}
	return 0
}

@[export: 'free']
@[unsafe]
fn __free(ptr voidptr) {
	if ptr == 0 {
		return
	}
	bare_heap_lock()
	unsafe { global_allocator.free_(ptr) }
	bare_heap_unlock()
}

fn vsprintf(str &char, format &char, ap &u8) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size uint, format &char, ap &u8) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

// not really needed
fn bare_read(buf &u8, count u64) (i64, Errno) {
	return sys_read(0, buf, count)
}

pub fn bare_print(buf &u8, len u64) {
	sys_write(1, buf, len)
}

fn bare_eprint(buf &u8, len u64) {
	sys_write(2, buf, len)
}

@[noreturn]
fn bare_panic(msg string) {
	eprintln('V panic: ' + msg)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

@[export: 'exit']
@[noreturn]
fn __exit(code int) {
	$if linux && (amd64 || arm64) {
		$if !v_bare_no_startup ? {
			bare_flush_streams()
		}
	}
	sys_exit(code)
}

// __qsort takes the C callback type: a V `FnSortCB` returns a 64-bit `int`.
@[export: 'qsort']
fn __qsort(base voidptr, nmemb uint, size uint, sort_cb C.qsort_callback_func) {
	$if linux && (amd64 || arm64) {
		$if v_bare_no_startup ? {
			C.qsort(base, nmemb, size, sort_cb)
		} $else {
			C.v_freestanding_qsort(base, nmemb, size, sort_cb)
		}
	} $else {
		panic('qsort() is unavailable on this freestanding target')
	}
}

fn init_global_allocator() {
	global_allocator = dlmalloc.new(get_linux_allocator())
	global_allocator_ready = true
}
