module builtin

//__global global_allocator dlmalloc.Dlmalloc

[unsafe]
pub fn __malloc(size usize) voidptr {
	unsafe {
		return C.malloc(int(size))
	}
}

[unsafe]
pub fn memcpy(dest &C.void, src &C.void, n usize) &C.void {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(src) }
	unsafe {
		for i in 0 .. int(n) {
			dest_[i] = src_[i]
		}
	}
	return unsafe { dest }
}

[unsafe]
fn strlen(_s &C.void) usize {
	s := unsafe { &u8(_s) }
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {}
	return usize(i)
}

[unsafe]
fn realloc(old_area &C.void, new_size usize) &C.void {
	if old_area == 0 {
		return unsafe { malloc(int(new_size)) }
	}
	if new_size == usize(0) {
		unsafe { free(old_area) }
		return 0
	}
	old_size := unsafe { *(&u64(old_area - sizeof(u64))) }
	if u64(new_size) <= old_size {
		return unsafe { old_area }
	} else {
		new_area := unsafe { malloc(int(new_size)) }
		unsafe { memmove(new_area, old_area, usize(old_size)) }
		unsafe { free(old_area) }
		return new_area
	}
}

[unsafe]
fn memset(s &C.void, c int, n usize) &C.void {
	mut s_ := unsafe { &char(s) }
	for i in 0 .. int(n) {
		unsafe {
			s_[i] = char(c)
		}
	}
	return unsafe { s }
}

[unsafe]
fn memmove(dest &C.void, src &C.void, n usize) &C.void {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(src) }
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

[export: 'calloc']
[unsafe]
fn __calloc(nmemb usize, size usize) &C.void {
	new_area := unsafe { malloc(int(nmemb) * int(size)) }
	unsafe { memset(new_area, 0, nmemb * size) }
	return new_area
}

fn getchar() int {
	return 0
}

fn memcmp(a &C.void, b &C.void, n usize) int {
	a_ := unsafe { &u8(a) }
	b_ := unsafe { &u8(b) }
	for i in 0 .. int(n) {
		if unsafe { a_[i] != b_[i] } {
			unsafe {
				return a_[i] - b_[i]
			}
		}
	}
	return 0
}

fn vsprintf(str &char, format &char, ap &byte) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size usize, format &char, ap &byte) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

enum Errno {
	enoerror
	eerror
}

// not really needed
fn bare_read(buf &byte, count u64) (i64, Errno) {
	return 0, Errno.eerror
}

pub fn bare_print(buf &byte, len u64) {
}

fn bare_eprint(buf &byte, len u64) {
}

pub fn write(_fd i64, _buf &byte, _count u64) i64 {
	return -1
}

[noreturn]
fn bare_panic(msg string) {
	println('V panic' + msg)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

[export: 'exit']
[noreturn]
fn __exit(code int) {
	unsafe {
		// the only way to abort process execution in WASM
		mut x := &int(nil)
		*x = code
	}
	for {}
}

[export: 'qsort']
fn __qsort(base voidptr, nmemb usize, size usize, sort_cb FnSortCB) {
	panic('qsort() is not yet implemented in `-freestanding`')
}

fn init_global_allocator() {
	// global_allocator = dlmalloc.new(get_wasm_allocator())
}
