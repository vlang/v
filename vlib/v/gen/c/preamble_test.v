module c

import os
import v.flat
import v.pref
import v.types

fn windows_preamble_test_gen() FlatGen {
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.target = pref.target_from('windows', 'amd64') or { panic(err) }
	return g
}

fn test_windows_translation_unit_preserves_configuration_preincludes() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"', '#include <bcrypt.h>',
		'#include <synchapi.h>', '#include <windows.h>']
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	config_index := c_code.index('#include "winapi_config.h"')?
	assert c_code.index('#ifndef UNICODE\n#define UNICODE\n#endif')? < config_index
	assert c_code.index('#ifndef _UNICODE\n#define _UNICODE\n#endif')? < config_index
	assert config_index < c_code.index('#include <windows.h>')?
	assert c_code.index('#include <windows.h>')? < c_code.index('#include <bcrypt.h>')?
	assert c_code.index('#include <windows.h>')? < c_code.index('#include <synchapi.h>')?
	assert c_code.count('#include <windows.h>') == 1
}

fn test_cross_c_translation_unit_guards_windows_unicode_apis() {
	for target_os in ['linux', 'windows'] {
		mut g := FlatGen.new()
		g.a = &flat.FlatAst{}
		g.set_target(pref.target_from(target_os, 'amd64') or { panic(err) })
		g.set_output_cross_c(true)
		g.preinclude_directives = ['#include "winapi_config.h"']
		g.emit_translation_unit_include_directives()
		c_code := g.sb.str()
		unicode_guard := '#if defined(_WIN32)\n#ifndef UNICODE\n#define UNICODE\n#endif\n#ifndef _UNICODE\n#define _UNICODE\n#endif\n#endif\n'
		assert c_code.starts_with(unicode_guard), target_os
		assert c_code.index('#include "winapi_config.h"')? >= unicode_guard.len
	}
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

fn test_windows_translation_unit_interposes_windows_header_before_dependent_headers() {
	mut g := windows_preamble_test_gen()
	g.preinclude_directives = ['#include "winapi_config.h"']
	g.add_c_directive('crypto.rand.internal', '#include <bcrypt.h>', false)
	g.add_c_directive('sync', '#include <synchapi.h>', false)
	g.add_c_directive('net', '#include <winsock2.h>', false)
	g.add_c_directive('net', '#include <ws2tcpip.h>', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	config_index := c_code.index('#include "winapi_config.h"')?
	winsock_index := c_code.index('#include <winsock2.h>')?
	ws2tcpip_index := c_code.index('#include <ws2tcpip.h>')?
	windows_index := c_code.index('#include <windows.h>')?
	bcrypt_index := c_code.index('#include <bcrypt.h>')?
	synchapi_index := c_code.index('#include <synchapi.h>')?
	assert config_index < winsock_index
	assert winsock_index < ws2tcpip_index
	assert ws2tcpip_index < windows_index
	assert windows_index < bcrypt_index
	assert windows_index < synchapi_index
	assert c_code.count('#include <windows.h>') == 1
}

// legacy_winsock_guarded is the C that keeps the windows.h of the Windows SDK from
// loading the legacy winsock.h while `directive` is included, and releases the macro
// again afterwards.
fn legacy_winsock_guarded(directive string) string {
	return '#if defined(_MSC_VER) && !defined(_WINSOCKAPI_)\n#define _WINSOCKAPI_\n#define V_LEGACY_WINSOCK_GUARD\n#endif\n${directive}\n#ifdef V_LEGACY_WINSOCK_GUARD\n#undef V_LEGACY_WINSOCK_GUARD\n#ifndef _WINSOCK2API_\n#undef _WINSOCKAPI_\n#endif\n#endif\n'
}

fn test_windows_translation_unit_keeps_legacy_winsock_out_of_headers_that_include_windows_header() {
	// builtin is ordered before every module that uses sockets, and with the Boehm GC
	// its <gc.h> includes windows.h itself. That loaded the legacy winsock.h first, and
	// MSVC then rejected `os`'s winsock2.h with `'sockaddr': 'struct' type redefinition`
	// for every program importing `os`.
	mut g := windows_preamble_test_gen()
	g.add_c_directive('builtin', '#include <gc.h>', false)
	g.add_c_directive('os', '#include <io.h>', false)
	g.add_c_directive('os', '#include <winsock2.h>', false)
	g.add_c_directive('net', '#include <ws2tcpip.h>', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	assert c_code.contains(legacy_winsock_guarded('#include <gc.h>') + '#include <io.h>\n'), c_code
	// No header moves: the guard alone keeps winsock.h out of gc.h's windows.h.
	gc_index := c_code.index('#include <gc.h>')?
	io_index := c_code.index('#include <io.h>')?
	winsock_index := c_code.index('#include <winsock2.h>')?
	ws2tcpip_index := c_code.index('#include <ws2tcpip.h>')?
	windows_index := c_code.index('#include <windows.h>')?
	assert gc_index < io_index
	assert io_index < winsock_index
	assert winsock_index < ws2tcpip_index
	assert ws2tcpip_index < windows_index
	assert c_code.count('#define _WINSOCKAPI_') == 1
	assert c_code.count('#include <winsock2.h>') == 1
	assert c_code.count('#include <windows.h>') == 1
}

fn test_windows_translation_unit_leaves_headers_alone_before_conditional_winsock_includes() {
	// Only the C preprocessor knows whether a conditional winsock2.h include is
	// active. When it is not, the headers after windows.h rely on the winsock.h that
	// it loads (`SOCKET`, `sockaddr_in`, ...), so nothing may be suppressed on a guess.
	conditional_includes := [
		['#if 0\n#include <winsock2.h>\n#endif'],
		['#ifdef USE_WINSOCK2', '#include <winsock2.h>', '#endif'],
		['#define FD_SETSIZE 1024', '#ifndef NO_WINSOCK2', '#include <ws2tcpip.h>', '#endif'],
	]
	for includes in conditional_includes {
		mut g := windows_preamble_test_gen()
		g.add_c_directive('builtin', '#include <gc.h>', false)
		g.add_c_directive('main', '#include <windows.h>', false)
		g.add_c_directive('main', '#include <project/uses_socket.h>', false)
		for directive in includes {
			g.add_c_directive('main', directive, false)
		}
		g.emit_translation_unit_include_directives()
		c_code := g.sb.str()
		assert !c_code.contains('WINSOCK_GUARD'), c_code
		assert !c_code.contains('_WINSOCKAPI_'), c_code
		assert c_code.contains('#include <gc.h>\n#include <windows.h>\n#include <project/uses_socket.h>\n'), c_code
	}
}

fn test_windows_translation_unit_guards_before_the_last_unconditional_winsock_include() {
	// A conditional include does not hide an unconditional one: winsock2.h is certain
	// to follow every header up to the latter.
	mut g := windows_preamble_test_gen()
	g.add_c_directive('builtin', '#include <gc.h>', false)
	g.add_c_directive('os', '#include <winsock2.h>', false)
	g.add_c_directive('term', '#include <windows.h>', false)
	g.add_c_directive('net', '#define FD_SETSIZE 1024', false)
	g.add_c_directive('net', '#include <winsock2.h>\n#include <ws2tcpip.h>', false)
	g.add_c_directive('main', '#include <gc/gc.h>', false)
	g.add_c_directive('main', '#if 0\n#include <winsock2.h>\n#endif', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	assert c_code.contains(legacy_winsock_guarded('#include <gc.h>')), c_code
	assert c_code.contains(legacy_winsock_guarded('#include <windows.h>')), c_code
	assert !c_code.contains(legacy_winsock_guarded('#include <gc/gc.h>')), c_code
	assert c_code.count('#define V_LEGACY_WINSOCK_GUARD') == 2
}

fn test_windows_translation_unit_keeps_winsock_prerequisite_headers_in_place() {
	// A header that configures Winsock (FD_SETSIZE, ...) has to stay ahead of the
	// winsock2.h it configures, with or without a windows.h-including header before it.
	for with_gc in [true, false] {
		mut g := windows_preamble_test_gen()
		if with_gc {
			g.add_c_directive('builtin', '#include <gc.h>', false)
		}
		g.add_c_directive('main', '#include <project/socket_config.h>', false)
		g.add_c_directive('main', '#define FD_SETSIZE 1024', false)
		g.add_c_directive('main', '#include <winsock2.h>', false)
		g.emit_translation_unit_include_directives()
		c_code := g.sb.str()
		assert c_code.contains('#include <project/socket_config.h>\n#define FD_SETSIZE 1024\n#include <winsock2.h>\n'), c_code
		assert c_code.contains(legacy_winsock_guarded('#include <gc.h>')) == with_gc, c_code
		assert c_code.contains('_WINSOCKAPI_') == with_gc, c_code
		if with_gc {
			assert c_code.index('#include <gc.h>')? < c_code.index('#include <project/socket_config.h>')?
		}
	}
}

fn test_windows_translation_unit_guards_every_windows_header_including_form() {
	mut g := windows_preamble_test_gen()
	g.add_c_directive('builtin', '#if defined(_WIN32)\n#include <gc/gc.h>\n#endif', false)
	g.add_c_directive('term', '#ifdef USE_CONSOLE', false)
	g.add_c_directive('term', '#include <windows.h>', false)
	g.add_c_directive('term', '#endif', false)
	// ws2tcpip.h includes winsock2.h itself.
	g.add_c_directive('net', '#include <ws2tcpip.h>', false)
	g.emit_translation_unit_include_directives()
	c_code := g.sb.str()
	block := legacy_winsock_guarded('#if defined(_WIN32)\n#include <gc/gc.h>\n#endif')
	// The guard stays inside the lifted context of the include that it protects.
	guarded := '#ifdef USE_CONSOLE\n' + legacy_winsock_guarded('#include <windows.h>') + '#endif\n'
	assert c_code.contains(block), c_code
	assert c_code.contains(guarded), c_code
	assert c_code.index(block)? < c_code.index(guarded)?
	assert c_code.index(guarded)? < c_code.index('#include <ws2tcpip.h>')?
	assert c_code.count('#include <windows.h>') == 1
}

fn test_translation_unit_leaves_headers_alone_without_a_later_winsock_include() {
	// Nothing to protect: no winsock2.h, or winsock2.h already ahead of gc.h.
	mut plain := windows_preamble_test_gen()
	plain.add_c_directive('builtin', '#include <gc.h>', false)
	plain.add_c_directive('os', '#include <io.h>', false)
	plain.emit_translation_unit_include_directives()
	plain_code := plain.sb.str()
	assert !plain_code.contains('WINSOCK'), plain_code
	assert !plain_code.contains('<winsock.h>'), plain_code

	mut ordered := windows_preamble_test_gen()
	ordered.add_c_directive('builtin', '#include <winsock2.h>', false)
	ordered.add_c_directive('builtin', '#include <gc.h>', false)
	ordered.emit_translation_unit_include_directives()
	ordered_code := ordered.sb.str()
	assert !ordered_code.contains('WINSOCK'), ordered_code
	assert !ordered_code.contains('<winsock.h>'), ordered_code

	mut linux := FlatGen.new()
	linux.a = &flat.FlatAst{}
	linux.target = pref.target_from('linux', 'amd64') or { panic(err) }
	linux.add_c_directive('builtin', '#include <gc.h>', false)
	linux.add_c_directive('os', '#include <winsock2.h>', false)
	linux.emit_translation_unit_include_directives()
	linux_code := linux.sb.str()
	assert !linux_code.contains('WINSOCK'), linux_code
	assert linux_code.index('#include <gc.h>')? < linux_code.index('#include <winsock2.h>')?
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
	// allocate the TLS index itself on first use - and exactly once, or a
	// second index would strand the storage threads already hold in the first.
	assert !windows_code.contains('__attribute__((constructor))')
	assert windows_code.contains('state_slot(void) { state_key_init();')
	// The ready check runs on every slot access; on x86 it must be a plain
	// load, not a locked read-modify-write shared by every thread.
	assert windows_code.contains('#if defined(__x86_64__) || defined(__i386__)\n#define state_key_is_ready() (*(volatile unsigned int*)&state_key_ready)\n#else\n#define state_key_is_ready() __atomic_add_fetch(&state_key_ready, 0, 5)\n#endif')
	assert windows_code.contains('if (state_key_is_ready()) { return; }')
	assert !windows_code.contains('__atomic_add_fetch(&state_key_ready, 0, 5)) { return; }')
	assert windows_code.contains('if (__atomic_add_fetch(&state_key_claim, 1, 5) == 1) {')
	assert windows_code.contains('__atomic_add_fetch(&state_key_ready, 1, 5);')
	assert windows_code.contains('while (!state_key_is_ready()) { Sleep(0); }')
	assert windows_code.contains('}\n#undef state_key_is_ready\n')
	// The key and the resolved Fls* pointers are only read after the publish.
	claim_index := windows_code.index('__atomic_add_fetch(&state_key_claim, 1, 5)')?
	publish_index := windows_code.index('__atomic_add_fetch(&state_key_ready, 1, 5)')?
	alloc_index := windows_code.index('fls_alloc(state_slot_free)')?
	assert claim_index < alloc_index
	assert alloc_index < publish_index
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

fn test_vinix_globals_do_not_require_elf_tls() {
	mut g := FlatGen.new()
	g.target = pref.target_from('vinix', 'arm64') or { panic(err) }
	g.global_modules['g_autostr_addr_state'] = 'builtin'
	assert !g.global_is_thread_local('g_autostr_addr_state')
	assert !g.global_is_thread_local('__anon_fn_1_capture')

	g.target = pref.target_from('linux', 'arm64') or { panic(err) }
	assert g.global_is_thread_local('g_autostr_addr_state')
	assert g.global_is_thread_local('__anon_fn_1_capture')
}

fn test_arena_stack_top_is_thread_local_only_in_builtin() {
	mut g := FlatGen.new()
	g.target = pref.target_from('linux', 'arm64') or { panic(err) }
	g.global_modules['g_arena_top'] = 'builtin'
	assert g.global_is_thread_local('g_arena_top')
	g.global_modules['g_arena_top'] = 'main'
	assert !g.global_is_thread_local('g_arena_top')
}

fn test_manual_stdlib_headers_clear_fortified_memory_macros() {
	headers := manual_stdlib_c_headers()
	for name in ['memcpy', 'memmove', 'memset'] {
		assert headers.contains('#ifdef ${name}\n#undef ${name}\n#endif'), name
	}
}

fn test_manual_stdlib_header_problem_accepts_the_embedded_header() {
	assert manual_stdlib_header_problem(manual_c_headers_source) == ''
	assert manual_stdlib_c_headers_error() == none
}

fn test_manual_stdlib_header_problem_reports_truncated_headers() {
	cut := manual_c_headers_source.index('};\n#endif') or { panic('no RAND_MAX enum close') }
	assert manual_stdlib_header_problem(manual_c_headers_source[..cut]).contains('unterminated `#ifndef RAND_MAX`')
	assert manual_stdlib_header_problem(manual_c_headers_source[..manual_c_headers_source.len - 1]).contains('newline')
	assert manual_stdlib_header_problem('#endif\n').contains('unmatched `#endif`')
}

fn test_manual_stdlib_header_problem_follows_preprocessor_comments_and_continuations() {
	for text in ['/*\n#if IGNORE\n*/\n', '#if(1)\n#endif/*closed*/\n',
		'# /* comment */ if 1\n# /* comment */ endif\n', '#i\\\nf 1\n#en\\\ndif\n',
		'// ignored \\\n#if ignored\n', 'char* comment = "/*";\n#if 1\n#endif\n',
		'#if 1\n#elif 2\n#else\n#if 3\n#else\n#endif\n#endif\n'] {
		assert manual_stdlib_header_problem(text) == '', text
	}
	assert manual_stdlib_header_problem('#if(1)\n').contains('unterminated')
	assert manual_stdlib_header_problem('/*\n#endif\n').contains('block comment')
	for text in ['#else\n', '#elif 1\n', '#if 1\n#else\n#else\n#endif\n', '#if 1\n#else\n#elif 2\n#endif\n'] {
		assert manual_stdlib_header_problem(text) != '', text
	}
}

fn test_manual_stdlib_header_usage_matches_headerless_and_target_header_modes() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
	ast.add_node(flat.Node{ kind: .module_decl, value: 'builtin' })
	ast.add_node(flat.Node{ kind: .directive, value: 'include', typ: '<pthread.h>' })
	assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
	ast.add_node(flat.Node{ kind: .directive, value: 'include', typ: '<stdio.h>' })
	assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
	prefs.target_libc_headers = true
	assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
}

fn test_manual_stdlib_header_usage_matches_include_collection() {
	for kind in ['include', 'preinclude', 'insert'] {
		for header in ['<stdio.h>', '"local_header.h"', 'windows <windows.h>'] {
			mut ast := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&ast)
			mut prefs := pref.new_preferences()
			prefs.target = pref.host_target()
			ast.add_node(flat.Node{ kind: .file, value: '/project/main.v' })
			ast.add_node(flat.Node{ kind: .module_decl, value: 'main' })
			ast.add_node(flat.Node{ kind: .directive, value: kind, typ: header })
			assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, []) == (header != 'windows <windows.h>'
				|| prefs.target.os == 'windows')
			// Portable C includes retain a preprocessor guard for the target C compiler.
			prefs.output_cross_c = true
			assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
			prefs.target_libc_headers = true
			assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [])
		}
	}
}

fn test_manual_stdlib_header_usage_resolves_native_sources_with_cli_include_dirs() {
	root := os.join_path(os.vtmp_dir(), 'manual_header_usage_${os.getpid()}')
	include_dir := os.join_path(root, 'includes')
	os.mkdir_all(include_dir)!
	defer { os.rmdir_all(root) or {} }
	for has_include in [false, true] {
		source := if has_include {
			'#include <stdio.h>\n'
		} else {
			'int helper(void) { return 1; }\n'
		}
		os.write_file(os.join_path(include_dir, 'helper.c'), source)!
		for portable in [false, true] {
			mut ast := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&ast)
			mut prefs := pref.new_preferences()
			prefs.target = pref.host_target()
			prefs.output_cross_c = portable
			ast.add_node(flat.Node{ kind: .file, value: os.join_path(root, 'main.v') })
			ast.add_node(flat.Node{ kind: .module_decl, value: 'main' })
			ast.add_node(flat.Node{ kind: .directive, value: 'include', typ: '"helper.c"' })
			assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, ['-I', include_dir]) == (!portable
				|| has_include)
		}
	}
}

fn test_manual_stdlib_headers_identify_gcc_without_matching_clang_or_tcc() {
	headers := manual_stdlib_c_headers()
	assert headers.contains('#if defined(__GNUC__) && !defined(__TINYC__) && !defined(__cplusplus) && !defined(__clang__)')
	assert headers.contains('#define __V_GCC__')
	assert headers.index('#define __V_GCC__')? < headers.index('defined(__V_GCC__)')?
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
	assert windows_code.contains('static __v_thread __v_thread_spawn_detached(__v_thread_start_fn start, void* arg, void (*cleanup)(void*)) {'), windows_code
	// The detached thread frees its own context and result; its handle is closed at once.
	assert windows_code.contains('__v_thread_free(raw_context); void* result = context.start(context.arg); if (result) __v_thread_free(result); return 0; }'), windows_code
	assert windows_code.contains('HANDLE handle = CreateThread(NULL, __v_thread_stack_size, __v_windows_detached_thread_start, context, 0, NULL);'), windows_code
	assert windows_code.contains('if (!CloseHandle(handle))'), windows_code
	assert !windows_code.contains('pthread_'), windows_code
	posix_code := c_code[posix_start..]
	assert posix_code.contains('pthread_equal(a.handle, b.handle) != 0'), posix_code
	assert posix_code.contains('pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)'), posix_code
	assert posix_code.contains('__v_thread_free(raw_context); void* result = context.start(context.arg); if (result) __v_thread_free(result); return NULL; }'), posix_code
}

fn test_headerless_thread_runtime_can_spawn_detached_threads() {
	mut g := FlatGen.new()
	g.headerless_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('int pthread_detach(void* thread);'), c_code
	assert c_code.contains('HANDLE handle = CreateThread(NULL, __v_thread_stack_size, __v_windows_detached_thread_start, context, 0, NULL);'), c_code
	assert c_code.contains('pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)'), c_code
}

fn test_cross_c_system_libc_preamble_keeps_posix_environ() {
	mut g := windows_preamble_test_gen()
	g.set_output_cross_c(true)
	g.system_libc_preamble()
	c_code := g.sb.str()
	assert c_code.contains('#ifndef _WIN32\nextern char** environ;\n#endif')
}

fn test_strict_iso_c_flags_request_linux_posix_feature_macros() {
	for flags in [['-std=c99'], ['-std=c11'], ['--std=c17'], ['-std', 'c99'], ['-ansi']] {
		mut g := FlatGen.new()
		g.c_flags = flags
		g.c99_feature_test_macros()
		c_code := g.sb.str()
		assert c_code.contains('#if defined(__linux__) && !defined(_GNU_SOURCE)\n#define _GNU_SOURCE\n#endif'), '${flags}: ${c_code}'
		assert c_code.contains('#define _POSIX_C_SOURCE 200809L'), '${flags}: ${c_code}'
	}
}

fn test_gnu_c_flags_keep_default_feature_macros() {
	for flags in [[]string{}, ['-std=gnu11'], ['-std=c99', '-std=gnu99'], ['-O2']] {
		mut g := FlatGen.new()
		g.c_flags = flags
		g.c99_feature_test_macros()
		c_code := g.sb.str()
		assert !c_code.contains('_GNU_SOURCE'), '${flags}: ${c_code}'
		assert !c_code.contains('_POSIX_C_SOURCE'), '${flags}: ${c_code}'
	}
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

fn test_target_libc_preamble_uses_target_header_declarations() {
	mut g := FlatGen.new()
	g.set_target_libc_headers(true)
	g.add_spawn_wrapper_def('static void closure_wrapper(void) {}')
	g.preamble()
	c_code := g.sb.str()
	for header in ['stdint.h', 'stddef.h', 'stdatomic.h', 'errno.h', 'fcntl.h', 'signal.h', 'stdio.h',
		'stdlib.h', 'string.h', 'math.h', 'time.h', 'unistd.h', 'sys/stat.h', 'sys/time.h'] {
		assert c_code.contains('#include <${header}>'), header
	}
	assert c_code.contains('#if __has_include(<stdatomic.h>)')
	assert c_code.contains('#if __has_include(<sys/stat.h>)')
	compat_guard := '#if defined(__OBJC__) && defined(__GNUC__) && !defined(__clang__)'
	assert c_code.contains('${compat_guard}\n#define _Atomic volatile\n#endif\n#include <stdatomic.h>')
	assert c_code.contains('#include <stdatomic.h>\n${compat_guard}\n#undef _Atomic\n#endif')
	assert !c_code.contains('#include <pthread.h>')
	assert c_code.contains('typedef uint64_t u64;')
	assert !c_code.contains('typedef long long time_t;')
	assert !c_code.contains('typedef __SIZE_TYPE__ size_t;')
	assert !c_code.contains('typedef __UINTPTR_TYPE__ uintptr_t;')
	assert !c_code.contains('typedef struct FILE FILE;')
	assert c_code.contains('int backtrace(void** __array, int __size);')
	assert c_code.contains('char** backtrace_symbols(void* const* __array, int __size);')
	assert c_code.contains('void backtrace_symbols_fd(void* const* __array, int __size, int __fd);')
	assert !c_code.contains('static __v_thread __v_thread_spawn(')
	for name in ['open', 'read', 'close', 'pipe', 'signal', 'sysconf', 'setbuf', 'fseeko', 'memmem',
		'mempcpy', 'chmod', 'lstat', 'mkdir', 'opendir', 'readdir', 'syscall', 'gettimeofday'] {
		assert !g.should_emit_c_extern_decl(name), name
	}
}

fn test_system_libc_preamble_uses_system_pointer_types() {
	mut g := FlatGen.new()
	g.add_c_directive('main', '#include <stdint.h>', false)
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('#include <stdint.h>')
	assert c_code.contains('#include <stddef.h>')
	assert c_code.contains('typedef uint64_t u64;')
	assert !c_code.contains('typedef __SIZE_TYPE__ size_t;')
	assert !c_code.contains('typedef __UINTPTR_TYPE__ uintptr_t;')
}

fn test_target_libc_preamble_emits_only_thread_type_for_type_only_usage() {
	mut g := FlatGen.new()
	g.set_target_libc_headers(true)
	g.needs_thread_type = true
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('#include <pthread.h>')
	assert c_code.contains('typedef struct { pthread_t handle; } __v_thread;')
	assert !c_code.contains('static __v_thread __v_thread_spawn(')
	assert !c_code.contains('static void* __v_thread_join(')
	assert !c_code.contains('pthread_equal(a.handle, b.handle)')
}

fn test_target_libc_preamble_emits_pthread_runtime_when_threads_are_used() {
	mut g := FlatGen.new()
	g.set_target_libc_headers(true)
	g.needs_thread_runtime = true
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('#include <pthread.h>')
	assert c_code.contains('static __v_thread __v_thread_spawn(')
	assert c_code.contains('static void* __v_thread_join(')
	assert c_code.contains('pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)')
	assert c_code.contains('pthread_equal(a.handle, b.handle) != 0')
	assert c_code.contains('void* p = GC_MALLOC_UNCOLLECTABLE(size);')
	assert c_code.contains('GC_FREE(ptr);')
}

fn test_vinix_target_libc_thread_runtime_uses_freestanding_pthread_abi() {
	mut g := FlatGen.new()
	g.target = pref.target_from('vinix', 'arm64') or { panic(err) }
	g.set_target_libc_headers(true)
	g.needs_thread_runtime = true
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('pthread_create(&result.handle, NULL, (void*)start, arg)')
	assert c_code.contains('pthread_create(&handle, NULL, (void*)__v_detached_thread_start, context)')
	assert c_code.contains('if (pthread_detach(handle) != 0) exit(1);')
	// Detached spawn wrappers free their argument block with it.
	assert c_code.contains('static void __v_thread_free(void* ptr) { free(ptr); }')
	assert !c_code.contains('pthread_attr_init(&attr)')
	assert !c_code.contains('fprintf(stderr, "V thread')
	assert !c_code.contains('abort();')
}

fn test_target_libc_preamble_includes_pthread_for_direct_calls_without_thread_runtime() {
	mut g := FlatGen.new()
	g.set_target_libc_headers(true)
	g.c_extern_refs['pthread_mutex_lock'] = true
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('#include <pthread.h>')
	assert !c_code.contains('static __v_thread __v_thread_spawn(')
}

fn test_target_libc_preamble_includes_pthread_for_pthread_backed_types() {
	mut g := FlatGen.new()
	g.set_target_libc_headers(true)
	g.needs_pthread_header = true
	g.preamble()
	c_code := g.sb.str()
	assert c_code.contains('#include <pthread.h>')
	assert !c_code.contains('typedef struct { pthread_t handle; } __v_thread;')
	assert !c_code.contains('static __v_thread __v_thread_spawn(')
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
	assert c_code.contains('#if defined(_WIN32) && (defined(__TINYC__) || (defined(_MSC_VER) && !defined(__clang__)))\n/* V atomic.h supplies atomic_thread_fence on Windows TCC and MSVC. */')
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
	assert c_code.contains('#if defined(__has_include)\n#if __has_include(<wchar.h>)\n#include <wchar.h>\n#endif\n#else\n#include <wchar.h>\n#endif')
	assert c_code.contains('#if defined(_WIN32) && (defined(__TINYC__) || (defined(_MSC_VER) && !defined(__clang__)))')
	assert c_code.contains('thirdparty/stdatomic/win/atomic.h"\n#else')
	compat_guard := '#if defined(__OBJC__) && defined(__GNUC__) && !defined(__clang__)'
	assert c_code.contains('${compat_guard}\n#define _Atomic volatile\n#endif\n#include <stdatomic.h>')
	assert c_code.contains('#include <stdatomic.h>\n${compat_guard}\n#undef _Atomic\n#endif')
}

fn test_system_libc_headers_leave_the_msvc_only_headers_to_the_c_preprocessor() {
	// The generated C is compiled by whatever C compiler the user picked, which is
	// not the one V generated it for: `vc/v_win.c` comes out of
	// `-cross -os windows -cc msvc` (gen_vc_ci.yml) and is then built by tcc, clang
	// and gcc (makev.bat). Choosing the MSVC-only headers at generation time baked
	// <intrin.h> and <dbghelp.h> into every Windows snapshot, and the bundled
	// TinyCC ships neither, so `makev.bat` died on
	// `include file 'intrin.h' not found` before compiling any line of V.
	// See #29146.
	mut g := FlatGen.new()
	// Generated exactly the way gen_vc_ci.yml generates the Windows snapshot.
	g.set_ccompiler('msvc')
	g.system_libc_headers()
	c_code := g.sb.str()
	// Only MSVC has these two, so the guard is the whole story: pinned as one block,
	// because a bare include of either one is exactly the regression.
	emitted := c_code.split_into_lines().filter(it.trim_space() in [
		'#include <intrin.h>',
		'#include <dbghelp.h>',
	]).map(it.trim_space())
	assert c_code_is_guarded_msvc_only(c_code), 'the snapshot emits ${emitted} unguarded, but only MSVC has those headers, so a snapshot built by tcc, clang or gcc fails with `include file not found`'
}

// c_code_is_guarded_msvc_only reports whether <intrin.h> and <dbghelp.h> are emitted
// as one `#if defined(_MSC_VER)` block, which is the only shape that lets a snapshot
// built by tcc, clang or gcc skip headers they do not have.
fn c_code_is_guarded_msvc_only(c_code string) bool {
	return c_code.contains('#if defined(_MSC_VER)\n#include <intrin.h>\n#include <dbghelp.h>\n#endif')
}

fn test_system_libc_headers_do_not_depend_on_the_c_compiler_v_was_generated_for() {
	// A snapshot is generated for one C compiler and compiled by another, so the
	// header set must not vary with `g.ccompiler`. Branches on it belong in the C
	// preprocessor instead: see #29146, where `if g.ccompiler == 'msvc'` made
	// `-cc msvc` the only spelling that could build a Windows snapshot, even
	// though <intrin.h>/<dbghelp.h> exist nowhere but under MSVC anyway.
	mut reference := []string{}
	for ccompiler in ['msvc', 'gcc', 'clang', 'tcc', 'tinyc'] {
		mut g := FlatGen.new()
		g.set_ccompiler(ccompiler)
		g.system_libc_headers()
		includes := g.sb.str().split_into_lines().filter(it.trim_space().starts_with('#include'))
			.map(it.trim_space())
		if reference.len == 0 {
			reference = includes.clone()
			continue
		}
		// Report the difference rather than both full lists: a header that is
		// missing from one spelling of the same snapshot is the whole finding.
		only_here := includes.filter(it !in reference)
		only_there := reference.filter(it !in includes)
		assert only_here.len == 0 && only_there.len == 0, '-cc ${ccompiler} emits ${only_here} but the msvc spelling emits ${only_there} instead'
	}
}

fn test_manual_windows_crt_declares_compiler_environment_setup() {
	headers := manual_stdlib_c_headers()
	declaration := 'V_CRT_LINKAGE int V_CRT_CALL _putenv_s(const char *name, const char *value);'
	assert headers.contains(declaration)
	mut g := windows_preamble_test_gen()
	g.compiler_vexe_env_setup = true
	g.compiler_vexe = 'C:/v/v.exe'
	g.compiler_vroot = 'C:/v'
	g.gen_compiler_vexe_env_setup()
	assert g.sb.str().contains('_putenv_s("VEXE", v3_vexe);')
}
