module builtin

[unsafe]
pub fn memcpy(dest &C.void, src &C.void, n size_t) &C.void {
	dest_ := unsafe { &byte(dest) }
	src_ := unsafe { &byte(src) }
	unsafe {
		for i in 0 .. int(n) {
			dest_[i] = src_[i]
		}
	}
	return dest
}

[export: 'malloc']
[unsafe]
fn __malloc(n size_t) &C.void {
	return unsafe { malloc(int(n)) }
}

[unsafe]
fn strlen(_s &C.void) size_t {
	s := unsafe { &byte(_s) }
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {}
	return size_t(i)
}

[unsafe]
fn realloc(old_area &C.void, new_size size_t) &C.void {
	if old_area == 0 {
		return unsafe { malloc(int(new_size)) }
	}
	if new_size == size_t(0) {
		unsafe { free(old_area) }
		return 0
	}
	old_size := unsafe { *(&u64(old_area - sizeof(u64))) }
	if u64(new_size) <= old_size {
		return old_area
	} else {
		new_area := unsafe { malloc(int(new_size)) }
		unsafe { memmove(new_area, old_area, size_t(old_size)) }
		unsafe { free(old_area) }
		return new_area
	}
}

[unsafe]
fn memset(s &C.void, c int, n size_t) &C.void {
	mut s_ := unsafe { &char(s) }
	for i in 0 .. int(n) {
		unsafe {
			s_[i] = char(c)
		}
	}
	return s
}

[unsafe]
fn memmove(dest &C.void, src &C.void, n size_t) &C.void {
	dest_ := unsafe { &byte(dest) }
	src_ := unsafe { &byte(src) }
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
	return dest
}

[export: 'calloc']
[unsafe]
fn __calloc(nmemb size_t, size size_t) &C.void {
	new_area := unsafe { malloc(int(nmemb) * int(size)) }
	unsafe { memset(new_area, 0, nmemb * size) }
	return new_area
}

fn getchar() int {
	x := byte(0)
	sys_read(0, &x, 1)
	return int(x)
}

fn memcmp(a &C.void, b &C.void, n size_t) int {
	a_ := unsafe { &byte(a) }
	b_ := unsafe { &byte(b) }
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
	err := mm_free(ptr)
	if err != .enoerror {
		eprintln('free error:')
		panic(err)
	}
}

fn vsprintf(str &char, format &char, ap &byte) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size size_t, format &char, ap &byte) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

// not really needed
fn bare_read(buf &byte, count u64) (i64, Errno) {
	return sys_read(0, buf, count)
}

fn bare_print(buf &byte, len u64) {
	sys_write(1, buf, len)
}

fn bare_eprint(buf &byte, len u64) {
	sys_write(2, buf, len)
}

fn bare_panic(msg string) {
	println('V panic' + msg)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

[export: 'exit']
fn __exit(code int) {
	sys_exit(code)
}
