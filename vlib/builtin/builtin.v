// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn exit(code int) {
	C.exit(code)
}

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

fn on_panic(f fn (int) int) {
	// TODO
}

pub fn print_backtrace() {
	return
	$if mac {
		buffer := [100]voidptr
		nr_ptrs := C.backtrace(buffer, 100)
		C.backtrace_symbols_fd(buffer, nr_ptrs, 1)
	}
}

pub fn panic(s string) {
	println('V panic: $s')
	print_backtrace()
	C.exit(1)
}

pub fn println(s string) {
	// Should never happen
	if isnil(s.str) {
		panic('println(NIL)')
	}
	C.printf('%.*s\n', s.len, s.str)
}

pub fn eprintln(s string) {
	if isnil(s.str) {
		panic('eprintln(NIL)')
	}
	$if mac {
		C.fprintf(stderr, '%.*s\n', s.len, s.str)
	}
	// TODO issues with stderr and cross compiling for Linux
	$else {
		println(s)
	}
}

pub fn print(s string) {
	C.printf('%.*s', s.len, s.str)
}

__global total_m i64 = 0
pub fn malloc(n int) byteptr {
	if n < 0 {
		panic('malloc(<0)')
	}
/* 
TODO 
#ifdef VPLAY
	if n > 10000 {
		panic('allocating more than 10 KB is not allowed in the playground')
	}
#endif
#ifdef DEBUG_ALLOC
	total_m += n
	println('\n\n\nmalloc($n) total=$total_m')
	print_backtrace()
#endif
*/ 
	ptr := C.malloc(n)
	if isnil(ptr) {
		panic('malloc($n) failed')
	}
	return ptr
}

pub fn calloc(n int) byteptr {
	if n < 0 {
		panic('calloc(<0)')
	}
	return C.calloc(n, 1)
}

fn _strlen(s byteptr) int {
	return C.strlen(s)
}


fn memdup(src voidptr, sz int) voidptr {
	mem := malloc(sz)
	return C.memcpy(mem, src, sz)
}



