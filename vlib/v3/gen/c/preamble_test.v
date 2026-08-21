module c

fn test_manual_stdlib_headers_clear_fortified_memory_macros() {
	headers := manual_stdlib_c_headers()
	for name in ['memcpy', 'memmove', 'memset'] {
		assert headers.contains('#ifdef ${name}\n#undef ${name}\n#endif'), name
	}
}

fn test_system_libc_thread_preamble_uses_native_windows_api() {
	mut g := FlatGen.new()
	g.system_libc_preamble()
	c_code := g.sb.str()
	windows_start := c_code.index('#ifdef _WIN32') or { panic('missing Windows guard') }
	posix_start := c_code.index('#else\ntypedef struct { pthread_t handle; } __v_thread;') or {
		panic('missing POSIX fallback')
	}
	windows_code := c_code[windows_start..posix_start]
	assert windows_code.contains('CreateThread('), windows_code
	assert windows_code.contains('WaitForSingleObject('), windows_code
	assert windows_code.contains('CloseHandle('), windows_code
	assert windows_code.contains('return a.handle == b.handle;'), windows_code
	assert !windows_code.contains('pthread_'), windows_code
	posix_code := c_code[posix_start..]
	assert posix_code.contains('pthread_equal(a.handle, b.handle) != 0'), posix_code
}

fn test_headerless_pthread_fallback_respects_darwin_type_guards() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	guard := c_code.all_before('typedef void* pthread_t;')
	assert guard.contains('!defined(_SYS__PTHREAD_TYPES_H_)'), guard
	assert guard.contains('!defined(_PTHREAD_T)'), guard
	assert c_code.contains('int pthread_equal(pthread_t t1, pthread_t t2);'), c_code
	assert c_code.contains('pthread_equal(a.handle, b.handle) != 0'), c_code
}

fn test_headerless_libc_preamble_declares_printf_for_cached_test_harnesses() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('int printf(const char* format, ...);'), c_code
}

fn test_headerless_libc_preamble_declares_qsort_for_generated_sort_helpers() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('void qsort(void* base, size_t items, size_t item_size, int (*cb)(const void*, const void*));'), c_code
}

fn test_headerless_platform_constants_include_process_errno_values() {
	mut g := FlatGen.new()
	g.headerless_platform_constants()
	c_code := g.sb.str()
	for definition in ['#define EPERM 1', '#define ESRCH 3', '#define EACCES 13'] {
		assert c_code.contains(definition), definition
	}
}

fn test_manual_stdlib_headers_define_l_tmpnam_for_glibc() {
	// The v3 backend embeds and reuses the v1 c_headers prelude (see manual_stdlib_c_headers).
	// Make sure the glibc L_tmpnam define is inherited, so a module header that pulls <stdio.h>
	// in on glibc still finds L_tmpnam; see https://github.com/vlang/v/issues/28108 .
	headers := manual_stdlib_c_headers()
	assert headers.contains('#if defined(__GLIBC__) || defined(__GNU_LIBRARY__)'), headers#[-500..]
	assert headers.contains('#ifndef L_tmpnam\n#define L_tmpnam 20\n#endif'), headers#[-500..]
}
