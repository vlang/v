module c

import v3.flat
import v3.pref

fn windows_preamble_test_gen() FlatGen {
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.target = pref.target_from('windows', 'amd64') or { panic(err) }
	return g
}

fn test_windows_translation_unit_preserves_configuration_preincludes() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"', '#include <synchapi.h>',
		'#include <windows.h>']
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	assert c_code.index('#include "winapi_config.h"')? < c_code.index('#include <windows.h>')?
	assert c_code.index('#include <windows.h>')? < c_code.index('#include <synchapi.h>')?
	assert c_code.count('#include <windows.h>') == 1
}

fn test_windows_translation_unit_adds_windows_header_after_configuration_preincludes() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"']
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	assert c_code.index('#include "winapi_config.h"')? < c_code.index('#include <windows.h>')?
}

fn test_windows_translation_unit_keeps_preserved_winsock_headers_before_windows_header() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"']
	g.add_c_directive('net', '#include <winsock2.h>', false)
	g.add_c_directive('net', '#include <ws2tcpip.h>', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	config_index := c_code.index('#include "winapi_config.h"')?
	winsock_index := c_code.index('#include <winsock2.h>')?
	ws2tcpip_index := c_code.index('#include <ws2tcpip.h>')?
	windows_index := c_code.index('#include <windows.h>')?
	assert config_index < winsock_index
	assert winsock_index < ws2tcpip_index
	assert ws2tcpip_index < windows_index
	assert c_code.count('#include <windows.h>') == 1
}

fn test_windows_translation_unit_interposes_windows_header_before_preserved_synchapi() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"']
	g.add_c_directive('sync', '#include <synchapi.h>', false)
	g.add_c_directive('net', '#include <winsock2.h>', false)
	g.add_c_directive('net', '#include <ws2tcpip.h>', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	config_index := c_code.index('#include "winapi_config.h"')?
	winsock_index := c_code.index('#include <winsock2.h>')?
	ws2tcpip_index := c_code.index('#include <ws2tcpip.h>')?
	windows_index := c_code.index('#include <windows.h>')?
	synchapi_index := c_code.index('#include <synchapi.h>')?
	assert config_index < winsock_index
	assert winsock_index < ws2tcpip_index
	assert ws2tcpip_index < windows_index
	assert windows_index < synchapi_index
	assert c_code.count('#include <windows.h>') == 1
}

fn test_thread_local_decl_uses_portable_c_dialects() {
	mut g := FlatGen.new()
	g.emit_thread_local_decl_after_tinyc('int state;')
	c_code := g.sb.str()
	assert c_code.contains('#elif defined(_MSC_VER)\n__declspec(thread) int state;')
	assert c_code.contains('#elif defined(__cplusplus)\nthread_local int state;')
	assert c_code.contains('#else\n_Thread_local int state;\n#endif')
}

fn test_tinyc_windows_thread_local_slot_uses_win32_tls() {
	mut g := FlatGen.new()
	g.emit_tinyc_windows_thread_local_slot('state', 'int', '')
	c_code := g.sb.str()
	windows_code := c_code.all_before('#elif defined(__TINYC__)')
	assert windows_code.contains('#if defined(__TINYC__) && defined(_WIN32)')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsAlloc")')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsGetValue")')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsSetValue")')
	assert windows_code.contains('fls_alloc(state_slot_free)')
	assert windows_code.contains('state_fls_get(state_key)')
	assert windows_code.contains('state_fls_set(state_key, p)')
	assert !windows_code.contains('FlsAlloc(state_slot_free)')
	assert !windows_code.contains('FlsGetValue(state_key)')
	assert !windows_code.contains('FlsSetValue(state_key, p)')
	assert windows_code.contains('state_slot_free(void* p) { free(p); }')
	assert !windows_code.contains('pthread_')
	// TinyCC never runs `__attribute__((constructor))`, so the slot has to
	// allocate the key itself on first use.
	assert !windows_code.contains('__attribute__((constructor))')
	assert windows_code.contains('if (state_key != 0xFFFFFFFF) { return; }')
	assert windows_code.contains('state_slot(void) { state_key_init();')
}

fn test_tinyc_pthread_value_slot_creates_its_own_key_lazily() {
	mut g := FlatGen.new()
	g.emit_tinyc_pthread_value_slot('state', 'State', '')
	c_code := g.sb.str()
	assert c_code.contains('static pthread_key_t state_key;')
	assert c_code.contains('static pthread_once_t state_key_once = PTHREAD_ONCE_INIT;')
	assert c_code.contains('static void state_key_create(void) { pthread_key_create(&state_key, free); }')
	assert c_code.contains('static void state_key_init(void) { pthread_once(&state_key_once, state_key_create); }')
	assert c_code.contains('static State* state_slot(void) { state_key_init(); void* p = pthread_getspecific(state_key);')
	assert c_code.contains('#define state (*state_slot())')
	// A constructor-initialized key stays 0 under TinyCC and aliases the first
	// key the process really creates, so one slot writes through another's
	// storage.
	assert !c_code.contains('__attribute__((constructor))')
}

fn test_tinyc_pthread_value_slot_supports_fixed_arrays() {
	mut g := FlatGen.new()
	g.emit_tinyc_pthread_value_slot('stack', 'i64', '[64]')
	c_code := g.sb.str()
	assert c_code.contains('static i64 (*stack_slot(void))[64] { stack_key_init(); void* p = pthread_getspecific(stack_key);')
	assert c_code.contains('calloc(1, sizeof(*stack_slot()))')
	assert !c_code.contains('__attribute__((constructor))')
}

fn test_tinyc_pthread_pointer_slot_does_not_rely_on_constructor() {
	mut g := FlatGen.new()
	g.emit_tinyc_pthread_pointer_slot('state', 'State*')
	c_code := g.sb.str()
	assert c_code.contains('static pthread_once_t state_key_once = PTHREAD_ONCE_INIT;')
	assert c_code.contains('static void state_key_create(void) { pthread_key_create(&state_key, 0); }')
	assert c_code.contains('static void state_key_init(void) { pthread_once(&state_key_once, state_key_create); }')
	assert c_code.contains('state_key_init();')
	assert !c_code.contains('__attribute__((constructor))')
}

fn test_autostr_thread_local_matching_is_restricted_to_builtin_global() {
	mut g := FlatGen.new()
	g.global_modules['g_autostr_addr_state'] = 'builtin'
	g.global_modules['foo.g_autostr_addr_state'] = 'foo'
	assert g.is_builtin_autostr_addr_state('g_autostr_addr_state')
	assert !g.is_builtin_autostr_addr_state('foo.g_autostr_addr_state')
	g.global_modules['g_autostr_addr_state'] = 'main'
	assert !g.is_builtin_autostr_addr_state('g_autostr_addr_state')
}

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
	assert c_code.contains('#if defined(__APPLE__) && defined(_SYS__PTHREAD_TYPES_H_)'), c_code
	assert c_code.contains('#define V_HEADERLESS_DARWIN_PTHREAD_TYPES 1'), c_code
	assert c_code.contains('typedef __darwin_pthread_t pthread_t;'), c_code
	assert c_code.contains('typedef __darwin_pthread_key_t pthread_key_t;'), c_code
	assert c_code.contains('#define PTHREAD_MUTEX_INITIALIZER { 0x32AAABA7, { 0 } }'), c_code
	assert c_code.contains('#define PTHREAD_ONCE_INIT { 0x30B1BCBA, { 0 } }'), c_code
	assert c_code.contains('int pthread_once(pthread_once_t* once_control, void (*init_routine)(void));'), c_code
	assert c_code.contains('int pthread_equal(pthread_t t1, pthread_t t2);'), c_code
	assert c_code.contains('pthread_equal(a.handle, b.handle) != 0'), c_code
}

fn test_headerless_libc_preamble_declares_printf_for_cached_test_harnesses() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('int printf(const char* format, ...);'), c_code
	assert c_code.contains('void perror(const char* message);'), c_code
	assert c_code.contains('void* memchr(const void* s, int c, size_t n);'), c_code
	assert c_code.contains('DWORD WINAPI TlsAlloc(void);'), c_code
	assert c_code.contains('void* WINAPI TlsGetValue(DWORD index);'), c_code
	assert c_code.contains('BOOL WINAPI TlsSetValue(DWORD index, void* value);'), c_code
	assert c_code.contains('void* WINAPI GetModuleHandleA(const char* module_name);'), c_code
	assert c_code.contains('void* WINAPI GetProcAddress(void* module, const char* proc_name);'), c_code
	assert !c_code.contains('DWORD WINAPI FlsAlloc('), c_code
	assert !c_code.contains('void* WINAPI FlsGetValue('), c_code
	assert !c_code.contains('BOOL WINAPI FlsSetValue('), c_code
}

fn test_headerless_libc_preamble_declares_qsort_for_generated_sort_helpers() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('void qsort(void* base, size_t items, size_t item_size, int (*cb)(const void*, const void*));'), c_code
}

fn test_headerless_linux_stat_preamble_supports_s390x() {
	mut g := FlatGen.new()
	g.headerless_linux_stat_struct()
	c_code := g.sb.str()
	s390_guard := '#elif defined(__s390x__)'
	s390_layout := 'struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u32 st_mode; u32 st_uid; u32 st_gid; int __glibc_reserved0; u64 st_rdev; i64 st_size; i64 st_atime; unsigned long st_atimensec; i64 st_mtime; unsigned long st_mtimensec; i64 st_ctime; unsigned long st_ctimensec; i64 st_blksize; i64 st_blocks; i64 __glibc_reserved[3]; };'
	assert c_code.contains('${s390_guard}\n${s390_layout}'), c_code
}

fn test_arch_macros_cover_all_supported_targets() {
	mut g := FlatGen.new()
	g.write_arch_macros()
	c_code := g.sb.str()
	for architecture, id in {
		'amd64':       1
		'arm64':       2
		'arm32':       3
		'rv64':        4
		'rv32':        5
		'x86':         6
		's390x':       7
		'ppc64le':     8
		'loongarch64': 9
		'sparc64':     10
		'ppc64':       11
		'ppc':         12
	} {
		assert c_code.contains('#define __V_${architecture} 1'), architecture
		assert c_code.contains('#define __V_architecture ${id}'), architecture
	}
}

fn test_libc_compat_gettid_supports_s390x() {
	mut g := FlatGen.new()
	g.libc_compat_fns['gettid'] = true
	g.libc_compat_decls()
	c_code := g.sb.str()
	assert c_code.contains('#elif defined(__s390x__)\n#define SYS_gettid 236'), c_code
}

fn test_headerless_libc_preamble_suppresses_its_mach_timebase_declaration() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	assert !g.should_emit_c_extern_decl('mach_timebase_info')
	// Compiler builtins never get a prototype; clang rejects redeclaring them.
	assert !g.should_emit_c_extern_decl('__atomic_fetch_add')
	assert !g.should_emit_c_extern_decl('__builtin_expect')
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

fn test_builtin_abi_decls_reuse_tcc_x64_stdatomic_fence_declaration() {
	mut g := FlatGen.new()
	g.atomic_thread_fence_compat_decls()
	c_code := g.sb.str()
	assert c_code.contains('#if defined(_WIN32) && defined(__TINYC__)\n/* V atomic.h supplies atomic_thread_fence on Windows TCC. */')
	assert c_code.contains('#define atomic_thread_fence(order) __atomic_thread_fence(order)')
	assert !c_code.contains('extern void __atomic_thread_fence(int order);')
}

fn test_map_equality_fallback_does_not_infer_value_type_from_size() {
	mut g := FlatGen.new()
	g.map_equality_fallback_decls()
	c_code := g.sb.str()
	assert c_code.contains('v3_map_value_eq(void* a, void* b, int value_bytes) { return memcmp(a, b, value_bytes) == 0; }')
	assert !c_code.contains('value_bytes == sizeof(map)')
	assert !c_code.contains('value_bytes == sizeof(string)')
}

fn test_builtin_heap_tracking_fallbacks_do_not_redefine_user_hooks() {
	mut fallback := FlatGen.new()
	fallback.heap_tracking_fallback_decls()
	assert fallback.sb.str().contains('__attribute__((weak)) void vheap_alloc')

	mut tracked := FlatGen.new()
	tracked.set_track_heap(true)
	tracked.heap_tracking_fallback_decls()
	assert tracked.sb.len == 0
}

fn test_system_libc_headers_make_stdatomic_compatible_with_gnu_objective_c() {
	mut g := FlatGen.new()
	g.system_libc_headers()
	c_code := g.sb.str()
	assert c_code.contains('#if defined(_WIN32) && defined(__TINYC__)')
	assert c_code.contains('thirdparty/stdatomic/win/atomic.h"\n#else')
	compat_guard := '#if defined(__OBJC__) && defined(__GNUC__) && !defined(__clang__)'
	assert c_code.contains('${compat_guard}\n#define _Atomic volatile\n#endif\n#include <stdatomic.h>')
	assert c_code.contains('#include <stdatomic.h>\n${compat_guard}\n#undef _Atomic\n#endif')
}
