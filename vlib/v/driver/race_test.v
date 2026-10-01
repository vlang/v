module driver

import os
import v.pref

fn test_race_builds_use_the_c_allocator() {
	assert v3_race_gc_mode('')! == 'none'
	assert v3_race_gc_mode('none')! == 'none'
	for gc_mode in ['boehm', 'boehm_full_opt', 'boehm_incr', 'vgc'] {
		if _ := v3_race_gc_mode(gc_mode) {
			assert false, '`-race -gc ${gc_mode}` must be rejected'
		} else {
			assert err.msg().contains('`-gc ${gc_mode}`'), err.msg()
		}
	}
}

fn test_race_rejects_the_arena_allocator() {
	v3_race_check_prealloc(['debug', 'race'])!
	for defines in [['prealloc'], ['race', 'prealloc=1']] {
		if _ := v3_race_check_prealloc(defines) {
			assert false, defines.str()
		} else {
			assert err.msg().contains('`-prealloc`'), err.msg()
		}
	}
}

fn test_the_race_define_is_reserved_for_race_builds() {
	v3_race_check_reserved_define(false, ['debug', 'racer'])!
	v3_race_check_reserved_define(true, ['race'])!
	for defines in [['race'], ['race=1']] {
		if _ := v3_race_check_reserved_define(false, defines) {
			assert false, defines.str()
		} else {
			assert err.msg().contains('use `-race`'), err.msg()
		}
	}
}

fn test_race_needs_the_c_backend() {
	v3_race_check_backend('c')!
	for backend in ['fastc', 'arm64', 'wasm', 'js'] {
		if _ := v3_race_check_backend(backend) {
			assert false, backend
		}
	}
}

fn test_race_targets_follow_the_thread_sanitizer_platforms() {
	for target_os, target_arch in {
		'linux':   'amd64'
		'macos':   'arm64'
		'freebsd': 'amd64'
	} {
		v3_race_check_target(pref.target_from(target_os, target_arch)!, false)!
	}
	v3_race_check_target(pref.target_from('linux', 'arm64')!, false)!
	for target_os, target_arch in {
		'windows': 'amd64'
		'linux':   'arm32'
		'openbsd': 'amd64'
	} {
		if _ := v3_race_check_target(pref.target_from(target_os, target_arch)!, false) {
			assert false, '${target_os}/${target_arch}'
		} else {
			assert err.msg().contains('not supported on ${target_os}/${target_arch}'), err.msg()
		}
	}
	if _ := v3_race_check_target(pref.target_from('linux', 'amd64')!, true) {
		assert false, 'portable cross output cannot load a TSan runtime'
	}
}

fn test_race_rejects_c_compilers_without_thread_sanitizer() {
	v3_race_check_c_compiler('clang', 'clang')!
	v3_race_check_c_compiler('/usr/bin/gcc-13', 'gcc')!
	if _ := v3_race_check_c_compiler('/v/thirdparty/tcc/tcc.exe', 'tinyc') {
		assert false, 'tcc has no ThreadSanitizer'
	} else {
		assert err.msg().contains('not `tcc.exe`'), err.msg()
	}
	if _ := v3_race_check_c_compiler('cl', 'msvc') {
		assert false, 'MSVC has no ThreadSanitizer'
	}
}

fn test_race_never_selects_tcc_implicitly() {
	host := pref.host_target()
	if host.os == 'windows' {
		// There is no race detector for Windows targets; `run` rejects them earlier.
		return
	}
	options := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     host.os
		host_target: host
		target:      host
		bundled_tcc: os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		race:        true
	}
	assert !v3_should_probe_bundled_tcc(options)
	selection := v3_select_c_compiler(@VEXEROOT, options)
	assert selection.implicit_tcc == ''
	assert selection.effective_c_compiler != 'tinyc'
}

fn test_race_c_compiler_hint_names_the_missing_runtime() {
	assert v3_race_c_compiler_hint('/usr/bin/ld: cannot find -ltsan: No such file or directory').contains('libtsan')
	assert v3_race_c_compiler_hint("clang: error: unsupported option '-fsanitize=thread' for target 'x'").contains('ThreadSanitizer')
	assert v3_race_c_compiler_hint('src.c:1:1: error: expected expression') == ''
}

fn test_windows_race_build_never_probes_bundled_tcc_implicitly() {
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     'windows'
		host_target: target
		target:      target
		bundled_tcc: os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	}
	assert v3_should_probe_bundled_tcc(base)
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		race: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		race:       true
		is_c_debug: true
	})
	// Explicit compiler requests are validated separately by the race driver.
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		race:                true
		c_compiler:          'tcc'
		c_compiler_explicit: true
	})
}
