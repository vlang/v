module builtin

[unsafe]
pub fn memcpy(dest &byte, src &byte, n int) &byte {
	unsafe {
		for i in 0 .. n {
			dest[i] = src[i]
		}
		return dest
	}
}

[export: 'malloc']
[unsafe]
fn __malloc(n int) &byte {
	return unsafe { malloc(n) }
}

[unsafe]
fn strlen(s &byte) int {
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {}
	return i
}

[unsafe]
fn realloc(old_area &byte, new_size int) &byte {
	if old_area == 0 {
		return unsafe { malloc(new_size) }
	}
	if new_size == 0 {
		unsafe { free(old_area) }
		return 0
	}
	old_size := unsafe { *(&u64(old_area - sizeof(u64))) }
	if new_size <= old_size {
		return old_area
	} else {
		new_area := unsafe { malloc(new_size) }
		unsafe { memmove(new_area, old_area, int(old_size)) }
		unsafe { free(old_area) }
		return new_area
	}
}

[unsafe]
fn memset(_s &byte, _c int, n int) &byte {
	c := char(_c)
	mut s := unsafe { &char(_s) }
	for i in 0 .. int(n) {
		unsafe {
			s[i] = c
		}
	}
	return s
}

[unsafe]
fn memmove(dest &byte, src &byte, n int) &byte {
	mut temp_buf := unsafe { malloc(n) }
	for i in 0 .. int(n) {
		unsafe {
			temp_buf[i] = src[i]
		}
	}

	for i in 0 .. int(n) {
		unsafe {
			dest[i] = temp_buf[i]
		}
	}
	unsafe { free(temp_buf) }
	return dest
}

[export: 'calloc']
[unsafe]
fn __calloc(nmemb int, size int) &byte {
	new_area := unsafe { malloc(nmemb * size) }
	unsafe { memset(new_area, 0, nmemb * size) }
	return new_area
}

fn getchar() int {
	x := byte(0)
	sys_read(C.stdin, &x, 1)
	return int(x)
}

fn memcmp(a &byte, b &byte, n int) int {
	for i in 0 .. int(n) {
		if unsafe { a[i] != b[i] } {
			unsafe {
				return a[i] - b[i]
			}
		}
	}
	return 0
}

[export: 'free']
[unsafe]
fn __free(ptr &byte) {
	err := mm_free(ptr)
	if err != .enoerror {
		eprintln('free error:')
		panic(err)
	}
}

fn vsprintf(str &char, format &char, ap &byte) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size int, format &char, ap &byte) int {
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
