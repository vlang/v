module builtin

fn builtin_init() {
	// Do nothing
}

[unsafe]
pub fn memcpy(dest0 &C.void, src0 &C.void, n int) &C.void {
	unsafe {
		mut dest := &byte(dest0)
		src := &byte(src0)
		for i in 0 .. int(n) {
			dest[i] = src[i]
		}
		return dest0
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
fn realloc(old_area &C.void, new_size int) &C.void {
	if old_area == 0 {
		return unsafe { malloc(new_size) }
	}
	if new_size == 0 {
		unsafe { free(old_area) }
		return 0
	}
	mlcinfo := unsafe { &MallocInfo(old_area) }
	old_size := mlcinfo.size
	if new_size <= old_size {
		return old_area
	} else {
		new_area := unsafe { malloc(new_size) }
		unsafe { memmove(new_area, old_area, old_size) }
		unsafe { free(old_area) }
		return new_area
	}
}

[unsafe]
fn memset(_s &C.void, _c int, n int) &C.void {
	c := char(_c)
	mut s := unsafe { &char(_s) }
	for i in 0 .. int(n) {
		unsafe {
			s[i] = c
		}
	}
	return _s
}

[unsafe]
fn memmove(_dest &C.void, _src &C.void, n int) &C.void {
	mut dest := unsafe { &byte(_dest) }
	src := unsafe { &byte(_src) }
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
fn calloc(nmemb int, size int) &C.void {
	new_area := unsafe { malloc(nmemb * size) }
	unsafe { memset(new_area, 0, nmemb * size) }
	return new_area
}

fn toupper(c int) int {
	if c >= `a` && c <= `z` {
		return c - 32
	}
	return c
}

fn tolower(c int) int {
	if c >= `A` && c <= `Z` {
		return c + 32
	}
	return c
}

fn getchar() int {
	x := byte(0)
	sys_read(C.stdin, &x, 1)
	return int(x)
}

fn memcmp(_a &C.void, _b &C.void, n int) int {
	a := unsafe { &byte(_a) }
	b := unsafe { &byte(_b) }
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
fn __free(ptr voidptr) {
	assert mm_free(ptr) == .enoerror
}
