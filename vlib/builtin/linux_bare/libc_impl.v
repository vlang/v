module builtin

import dlmalloc

__global global_allocator dlmalloc.Dlmalloc

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

[export: 'malloc']
[unsafe]
fn __malloc(n usize) &C.void {
	return unsafe { global_allocator.malloc(n) }
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
	x := u8(0)
	sys_read(0, &x, 1)
	return int(x)
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

[export: 'free']
[unsafe]
fn __free(ptr &C.void) {
	unsafe {
		global_allocator.free_(ptr)
	}
}

fn vsprintf(str &char, format &char, ap &byte) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size usize, format &char, ap &byte) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

// not really needed
fn bare_read(buf &byte, count u64) (i64, Errno) {
	return sys_read(0, buf, count)
}

pub fn bare_print(buf &byte, len u64) {
	sys_write(1, buf, len)
}

fn bare_eprint(buf &byte, len u64) {
	sys_write(2, buf, len)
}

pub fn write(fd i64, buf &byte, count u64) i64 {
	x, _ := sys_write(fd, buf, count)
	return x
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
	sys_exit(code)
}

[export: 'qsort']
fn __qsort(base voidptr, nmemb usize, size usize, sort_cb FnSortCB) {
	panic('qsort() is not yet implemented in `-freestanding`')
}

fn init_global_allocator() {
	global_allocator = dlmalloc.new(get_linux_allocator())
}
