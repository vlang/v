module builtin

//__global global_allocator dlmalloc.Dlmalloc

// __malloc allocates size bytes of memory.
@[unsafe]
pub fn __malloc(size uint) voidptr {
	unsafe {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc.
			return C._aligned_malloc(size, 1)
		} $else {
			return C.malloc(int(size))
		}
	}
	return unsafe { nil }
}

// memcpy copies n bytes from const_src to dest.
@[unsafe]
pub fn memcpy(dest &C.void, const_src &C.void, n uint) &C.void {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	unsafe {
		for i in 0 .. int(n) {
			dest_[i] = src_[i]
		}
	}
	return unsafe { dest }
}

@[unsafe]
fn strlen(const_s &char) uint {
	s := unsafe { &u8(const_s) }
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {
	}
	return uint(i)
}

@[unsafe]
fn realloc(old_area &C.void, new_size uint) &C.void {
	if old_area == 0 {
		return unsafe { malloc(int(new_size)) }
	}
	if new_size == uint(0) {
		unsafe { free(old_area) }
		return 0
	}
	old_size := unsafe { *(&u64(old_area - sizeof(u64))) }
	if u64(new_size) <= old_size {
		return unsafe { old_area }
	} else {
		new_area := unsafe { malloc(int(new_size)) }
		unsafe { memmove(new_area, old_area, uint(old_size)) }
		unsafe { free(old_area) }
		return new_area
	}
}

@[unsafe]
fn memset(s &C.void, c int, n uint) &C.void {
	mut s_ := unsafe { &char(s) }
	for i in 0 .. int(n) {
		unsafe {
			s_[i] = char(c)
		}
	}
	return unsafe { s }
}

@[unsafe]
fn memmove(dest &C.void, const_src &C.void, n uint) &C.void {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	mut temp_buf := unsafe { malloc(int(n)) }
	for i in 0 .. int(n) {
		unsafe {
			temp_buf[i] = src_[i]
		}
	}

	for i in 0 .. int(n) {
		unsafe {
			dest_[i] = temp_buf[i]
		}
	}
	unsafe { free(temp_buf) }
	return unsafe { dest }
}

@[export: 'calloc']
@[unsafe]
fn __calloc(nmemb uint, size uint) &C.void {
	new_area := unsafe { malloc(int(nmemb) * int(size)) }
	unsafe { memset(new_area, 0, nmemb * size) }
	return new_area
}

fn getchar() int {
	return 0
}

fn memcmp(const_a &C.void, const_b &C.void, n uint) int {
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

fn vsprintf(str &char, format &char, ap &u8) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size uint, format &char, ap &u8) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

enum Errno {
	enoerror
	eerror
}

// not really needed
fn bare_read(buf &u8, count u64) (i64, Errno) {
	return 0, Errno.eerror
}

pub fn bare_print(buf &u8, len u64) {
}

fn bare_eprint(buf &u8, len u64) {
}

pub fn write(_fd i64, _buf &u8, _count u64) i64 {
	return -1
}

@[noreturn]
fn bare_panic(msg string) {
	println('V panic' + msg)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

@[export: 'exit']
@[noreturn]
fn __exit(code int) {
	unsafe {
		// the only way to abort process execution in WASM
		mut x := &int(nil)
		*x = code
	}
	for {}
}

@[export: 'qsort']
fn __qsort(base voidptr, nmemb uint, size uint, sort_cb FnSortCB) {
	panic('qsort() is not yet implemented in `-freestanding`')
}
