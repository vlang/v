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

pub fn malloc(n int) byteptr {
	if n < 0 {
		panic('malloc(<0)')
	}
#ifdef VPLAY
	if n > 10000 {
		panic('allocating more than 10 KB is not allowed in the playground')
	}
#endif
#ifdef DEBUG_ALLOC
	total := i64(0)
	# total_m += n;
	# total = total_m;
	println('\n\n\nmalloc($n) total=$total')
	print_backtrace()
#endif
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
	// TODO don't call a C function, implement it in V
	return C.strlen(s)
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr) Option {
	return Option {
		data: data
		ok: true
	}
}

fn memdup(src voidptr, sz int) voidptr {
	mem := malloc(sz)
	return C.memcpy(mem, src, sz)
}

pub fn error(s string) Option {
	return Option {
		error: s
	}
}

// TODO this is not used anymore
fn range_int(start, end int) []int {
	len := end - start
	mut res := [0; len]
	for i := 0; i < len; i++ {
		res[i] = start + i
	}
	return res
}

