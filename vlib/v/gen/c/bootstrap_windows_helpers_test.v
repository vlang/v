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
	assert !code.contains('#include "core_windows.h"'), 'the core facade must travel with the snapshot'
	assert !code.contains('#include "ticks_windows_api.h"'), 'the ticks facade must travel with the snapshot'
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

fn bootstrap_windows_run_c(cc string, name string, code string) ! {
	dir := os.join_path(os.vtmp_dir(), 'bootstrap_windows_${name}_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'probe.c')
	binary := os.join_path(dir, 'probe.exe')
	os.write_file(source, code)!
	compiled := os.exec([cc, '-std=c11', '-Werror=implicit-function-declaration',
		'-Werror=incompatible-pointer-types', source, '-o', binary])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([binary])
	assert ran.exit_code == 0, '${name}: ${ran.output}'
}

fn test_bootstrap_windows_acl_values_match_sdk() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	code := bootstrap_windows_test_gen().sb.str()
	acl := code.all_after('#define V_BOOTSTRAP_WIN_ACL\n').all_before('#endif')
	assert acl.len > 0
	// Pin the Win32 ABI independently of the headerless declarations. See
	// Microsoft's winnt.h: ACL_INFORMATION_CLASS, WELL_KNOWN_SID_TYPE and ACE types.
	// https://github.com/microsoft/win32metadata/blob/main/generation/WinSDK/RecompiledIdlHeaders/um/winnt.h
	mut assertions := ''
	for name, value in {
		'AclSizeInformation':                      2
		'WinCreatorOwnerSid':                      3
		'WinLocalSystemSid':                       22
		'WinBuiltinAdministratorsSid':             26
		'ACCESS_ALLOWED_ACE_TYPE':                 0
		'ACCESS_ALLOWED_COMPOUND_ACE_TYPE':        4
		'ACCESS_ALLOWED_OBJECT_ACE_TYPE':          5
		'ACCESS_ALLOWED_CALLBACK_ACE_TYPE':        9
		'ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE': 11
	} {
		assertions += '_Static_assert(${name} == ${value}, "${name} ABI");\n'
	}
	bootstrap_windows_run_c(cc, 'acl', 'typedef unsigned int DWORD; typedef unsigned short u16; typedef void* PSID;\n' +
		acl + assertions + 'int main(void) { return 0; }\n')!
}

fn test_bootstrap_windows_streams_reuse_crt_owned_file_objects() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	mut g := FlatGen.new()
	g.emit_bootstrap_windows_stdio()
	code := g.sb.str()
	emitted := bootstrap_windows_test_gen().sb.str()
	signature := 'void *v_builtin_windows_stream(unsigned int index) {'
	assert emitted.contains(signature)
	body := emitted.all_after(signature).all_before('\n}')
	stream_helper := signature + body + '\n}\n'
	// No descriptor or FILE allocation functions are linked into these probes.
	// The mock CRT owns the stream objects; all lookups must return those exact
	// addresses, including their existing buffers and descriptor associations.
	for variant in ['ucrt', 'tcc64', 'tcc32_dll', 'tcc32_crt'] {
		mut defines := '#define __cdecl\n'
		mut crt := ''
		mut expected := 'streams'
		if variant == 'ucrt' {
			crt = 'static int streams[3];\nvoid* __acrt_iob_func(unsigned int index) { return &streams[index]; }\n'
		} else {
			defines += '#define __TINYC__ 1\n'
			if variant == 'tcc64' {
				defines += '#define _WIN64 1\n'
				crt = 'static struct v_bootstrap_msvcrt_file streams[3];\nstruct v_bootstrap_msvcrt_file* __iob_func(void) { return streams; }\n'
			} else if variant == 'tcc32_crt' {
				defines += '#define _MSVCRT_ 1\n'
				crt = 'struct v_bootstrap_msvcrt_file _iob[3];\n'
				expected = '_iob'
			} else {
				crt = 'static struct v_bootstrap_msvcrt_file streams[3];\nstruct v_bootstrap_msvcrt_file (*_imp___iob)[] = &streams;\n'
			}
		}
		probe := 'int main(void) { for (int n = 0; n < 100000; ++n) { for (unsigned int i = 0; i < 3; ++i) { if (v_builtin_windows_stream(i) != &${expected}[i]) return 1; } } return 0; }\n'
		bootstrap_windows_run_c(cc, variant, defines + code + stream_helper + crt + probe)!
	}
}
