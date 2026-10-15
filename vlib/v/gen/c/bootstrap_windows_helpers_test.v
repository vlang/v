module c

import os

// The Windows bootstrap snapshot (`v -o vc/v_win.c -cross -os windows cmd/v`)
// links nothing external, so every native helper the compiler references has to
// arrive in the single C file. These tests pin that the four Windows core
// runtime sources arrive as definitions, not declarations.
//
// The failure this guards against published a snapshot that declared
// `v_toolcache_*`, `v_time_ticks_ms` and `v3_win_thread_*` but defined none of
// them, and every Windows CI job bootstrapping from it failed with
// `tcc: error: unresolved reference` before any program source was compiled.
fn bootstrap_windows_test_gen() &FlatGen {
	mut g := FlatGen.new()
	g.bootstrap_windows_runtime_helpers()
	return g
}

fn test_bootstrap_windows_helpers_define_the_toolcache_entries() {
	g := bootstrap_windows_test_gen()
	code := g.sb.str()
	// Each entry arrives with the parameter list its header spells, followed by
	// a body. A bare prototype would end the line with `;` instead.
	for name, params in {
		'v_toolcache_move_file_ex_w':       'const unsigned short *existing'
		'v_toolcache_create_file_w':        'const unsigned short *path'
		'v_toolcache_get_file_information': 'void *handle, void *information'
		'v_toolcache_close_handle':         'void *handle'
		'v_toolcache_file_is_disk':         'void *handle'
		'v_toolcache_read_file':            'void *handle, void *buffer'
		'v_toolcache_root_is_private':      'const unsigned short *path'
	} {
		signature := '${name}(${params}'
		start := code.index(signature) or { -1 }
		assert start >= 0, 'missing ${name}'
		tail := code[start + signature.len..]
		closing := tail.index(')') or { -1 }
		assert closing >= 0, 'unclosed ${name}'
		rest := tail[closing + 1..].trim_left(' \t\r\n')
		assert rest.starts_with('{'), '${name} is declared but never defined'
	}
}

fn test_bootstrap_windows_helpers_define_ticks_and_threads() {
	g := bootstrap_windows_test_gen()
	code := g.sb.str()
	assert code.contains('unsigned long long v_time_ticks_ms(void) {'), 'ticks helper has no body'
	assert code.contains('v3_win_thread_create('), 'missing v3_win_thread_create'
	assert code.contains('v3_win_thread_join('), 'missing v3_win_thread_join'
	assert code.contains('v3_win_thread_is_current('), 'missing v3_win_thread_is_current'
	assert code.contains('v_builtin_windows_stream('), 'missing v_builtin_windows_stream'
}

fn test_bootstrap_windows_helpers_carry_no_sdk_include() {
	g := bootstrap_windows_test_gen()
	code := g.sb.str()
	// The headerless path declares its own Win32 surface and the SDK spells
	// `DWORD` differently, so an SDK include here contradicts the preamble.
	assert !code.contains('#include <windows.h>'), 'an SDK include slipped into the bootstrap helpers'
	assert !code.contains('#include <aclapi.h>'), 'the aclapi include defeats the header fallback'
}

fn test_bootstrap_windows_helpers_force_the_acl_fallback() {
	g := bootstrap_windows_test_gen()
	code := g.sb.str()
	// With no `aclapi.h` in scope the header's own fallback declarations stand.
	// If the toolchain's header were included instead, its `DWORD` would clash
	// with the headerless preamble further down.
	assert code.contains('SE_FILE_OBJECT'), 'the ACL fallback never engaged'
}

// The toolcache scope macro must be emitted empty before the header: the
// header defines it `static` when unset, and cgen generates non-static
// prototypes for these symbols from the `fn C.` declarations, so a `static`
// definition would clash with them.
fn test_bootstrap_windows_toolcache_scope_stays_empty() {
	g := bootstrap_windows_test_gen()
	code := g.sb.str()
	define_pos := code.index('#define V_TOOLCACHE_WINDOWS_SCOPE\n') or { -1 }
	assert define_pos >= 0, 'the empty scope macro is missing'
	static_pos := code.index('#define V_TOOLCACHE_WINDOWS_SCOPE static') or { -1 }
	// The header text carries the `static` fallback, but it sits inside the
	// `#ifndef` the line above defeats, so it never takes effect.
	if static_pos >= 0 {
		assert static_pos > define_pos, 'the static fallback precedes the empty definition'
	}
}

fn test_bootstrap_windows_createfile_matches_os_spelling() {
	// `vlib/builtin/cfns.c.v` once spelled the security-attributes argument
	// `&u16` while `vlib/os` spelled it `voidptr`. Both prototypes reach the
	// same snapshot, so they must agree; the C type is opaque either way.
	builtin := os.read_file(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'cfns.c.v')) or {
		panic(err)
	}
	assert builtin.contains('lpSecurityAttributes voidptr'), 'builtin CreateFile spelling drifted from vlib/os'
}
