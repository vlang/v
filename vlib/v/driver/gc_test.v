module driver

import os
import v.pref

fn test_missing_bundled_gc_library_checks_only_link_inputs() {
	root := os.join_path(os.vtmp_dir(), 'v_missing_gc_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'thirdparty', 'tcc', 'lib'))!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for name in ['libgc.a', 'libgc.dylib'] {
		path := os.join_path(root, 'thirdparty', 'tcc', 'lib', name)
		assert v3_missing_bundled_gc_library([path], root) or { '' } == path
		for option in ['-I', '-L', '-include', '-o', '-x'] {
			assert v3_missing_bundled_gc_library([option, path], root) == none
		}
		assert v3_missing_bundled_gc_library(['-I${path}'], root) == none
		os.write_file(path, 'existing library')!
		assert v3_missing_bundled_gc_library([path], root) == none
	}
	assert v3_missing_bundled_gc_library([os.join_path(root, 'other', 'libgc.a')], root) == none
	assert v3_missing_bundled_gc_library(['-lgc'], root) == none
	assert v3_missing_c_library_name("ld: library 'gc' not found") or { '' } == 'gc'
	assert v3_missing_c_library_name('ld: library not found for -lgc') or { '' } == 'gc'
	assert v3_missing_c_library_name('/usr/bin/ld: cannot find -lobjc: No such file or directory') or {
		''
	} == 'objc'
	assert v3_missing_c_library_name('/usr/bin/ld: cannot find -l:libfoo.a: No such file or directory') or {
		''
	} == ':libfoo.a'
	message := v3_missing_gc_library_message('missing libgc.a')
	assert message.contains('Boehm GC library `missing libgc.a` was not found.')
	assert message.contains('-d use_bundled_libgc')
	assert message.contains('-gc none')
	os.rmdir_all(os.join_path(root, 'thirdparty'))!
	missing := os.join_path(root, 'thirdparty', 'tcc', 'lib', 'libgc.a')
	assert v3_missing_bundled_gc_library([missing], root) or { '' } == missing
}

fn test_v3_gc_mode_defines() {
	cases := {
		'':               ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm':          ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm_full':     ['gcboehm', 'gcboehm_full']
		'boehm_incr':     ['gcboehm', 'gcboehm_incr']
		'boehm_full_opt': ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm_incr_opt': ['gcboehm', 'gcboehm_incr', 'gcboehm_opt']
		'boehm_leak':     ['gcboehm', 'gcboehm_leak']
		'none':           []string{}
		'vgc':            ['vgc']
	}
	for mode, expected in cases {
		actual := v3_gc_mode_defines(mode, false) or { panic(err) }
		assert actual == expected
	}
}

fn test_v3_gc_mode_is_disabled_when_requested() {
	for mode in ['', 'boehm', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt',
		'boehm_leak', 'none', 'vgc'] {
		actual := v3_gc_mode_defines(mode, true) or { panic(err) }
		assert actual == []
	}
}

fn test_v3_gc_is_disabled_for_cross_compilation() {
	host := pref.host_target()
	cross_os := if host.os == 'linux' { 'macos' } else { 'linux' }
	cross_arch := if host.arch == 'amd64' { 'arm64' } else { 'amd64' }
	for mode in ['', 'boehm', 'boehm_incr'] {
		assert !v3_gc_is_disabled(false, false, host.os, host.arch, 'c', mode)
		assert v3_gc_is_disabled(true, false, host.os, host.arch, 'c', mode)
		assert v3_gc_is_disabled(false, true, host.os, host.arch, 'c', mode)
		assert v3_gc_is_disabled(false, false, cross_os, host.arch, 'c', mode)
		assert v3_gc_is_disabled(false, false, host.os, cross_arch, 'c', mode)
		assert v3_gc_is_disabled(false, false, 'cross', host.arch, 'c', mode)
		// The native backend never has a collector, on any host and target.
		assert v3_gc_is_disabled(false, false, host.os, host.arch, 'arm64', mode)
		assert v3_gc_is_disabled(false, false, host.os, host.arch, 'x64', mode)
	}
}

fn test_vgc_is_kept_for_cross_compilation() {
	host := pref.host_target()
	cross_os := if host.os == 'windows' { 'linux' } else { 'windows' }
	cross_arch := if host.arch == 'amd64' { 'arm64' } else { 'amd64' }
	assert !v3_gc_is_disabled(false, false, host.os, host.arch, 'c', 'vgc')
	assert !v3_gc_is_disabled(false, false, cross_os, host.arch, 'c', 'vgc')
	assert !v3_gc_is_disabled(false, false, host.os, cross_arch, 'c', 'vgc')
	// The compiler itself, portable C output and the native backend have no collector.
	assert v3_gc_is_disabled(true, false, host.os, host.arch, 'c', 'vgc')
	assert v3_gc_is_disabled(false, true, host.os, host.arch, 'c', 'vgc')
	assert v3_gc_is_disabled(false, false, host.os, host.arch, 'arm64', 'vgc')
	assert v3_gc_is_disabled(false, false, host.os, host.arch, 'x64', 'vgc')
}

fn test_v3_gc_mode_keeps_dynamic_boehm_define() {
	mut defines := ['dynamic_boehm']
	mut values := {
		'dynamic_boehm': 'true'
	}
	apply_v3_gc_mode('boehm', false, mut defines, mut values) or { panic(err) }
	assert defines == ['dynamic_boehm', 'gcboehm', 'gcboehm_full', 'gcboehm_opt']
	assert values['dynamic_boehm'] == 'true'
	assert values['gcboehm'] == 'true'
	assert values['gcboehm_full'] == 'true'
	assert values['gcboehm_opt'] == 'true'
}

fn test_v3_gc_mode_rejects_unknown_mode() {
	v3_gc_mode_defines('bogus', false) or {
		assert err.msg().contains('unknown garbage collection mode `-gc bogus`')
		return
	}
	assert false
}

fn test_v3_gc_without_thread_local_allocation_uses_source_collector() {
	for define in ['no_gc_thread_local_alloc', 'no_gc_thread_local_alloc=true'] {
		mut defines := []string{}
		mut values := map[string]string{}
		record_user_define(mut defines, mut values, define)
		apply_v3_gc_mode('boehm', false, mut defines, mut values)!
		assert 'use_bundled_libgc' in defines
		assert values['use_bundled_libgc'] == 'true'
	}
}

fn test_v3_gc_precise_flag_preserves_dynamic_and_disabled_collector_modes() {
	for mode in ['none', 'boehm'] {
		mut defines := ['no_gc_thread_local_alloc', 'dynamic_boehm']
		mut values := map[string]string{}
		apply_v3_gc_mode(mode, false, mut defines, mut values)!
		assert 'use_bundled_libgc' !in defines
	}
	for disabled in [false, true] {
		mut defines := ['no_gc_thread_local_alloc']
		mut values := map[string]string{}
		apply_v3_gc_mode(if disabled { 'boehm' } else { 'none' }, disabled, mut defines, mut values)!
		assert 'use_bundled_libgc' !in defines
	}
}

fn test_precise_gc_prefers_platform_cc_except_for_dynamic_boehm() {
	assert v3_gc_prefers_platform_cc(['gcboehm', 'no_gc_thread_local_alloc'])
	assert v3_gc_prefers_platform_cc(['gcboehm', 'no_gc_thread_local_alloc',
		'no_gc_thread_local_alloc=true'])
	assert !v3_gc_prefers_platform_cc([])
	assert !v3_gc_prefers_platform_cc(['no_gc_thread_local_alloc'])
	assert !v3_gc_prefers_platform_cc(['gcboehm', 'no_gc_thread_local_alloc', 'dynamic_boehm'])
}

fn test_precise_gc_respects_empty_define_values() {
	for empty in ['no_gc_thread_local_alloc', 'dynamic_boehm'] {
		mut defines := []string{}
		mut values := map[string]string{}
		if empty == 'dynamic_boehm' {
			record_user_define(mut defines, mut values, 'no_gc_thread_local_alloc')
		}
		record_user_define(mut defines, mut values, '${empty}=')
		apply_v3_gc_mode('boehm', false, mut defines, mut values)!
		assert v3_gc_prefers_platform_cc(defines) == (empty == 'dynamic_boehm')
		assert ('use_bundled_libgc' in defines) == (empty == 'dynamic_boehm')
	}
}

fn test_precise_gc_keeps_explicit_c_compiler_selection() {
	linux_target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend:            'c'
		c_compiler:         'cc'
		host_os:            'linux'
		host_target:        linux_target
		target:             linux_target
		prefer_platform_cc: true
	}
	assert !v3_should_probe_bundled_tcc(base)
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          'tcc'
		c_compiler_explicit: true
	})
	selection := v3_select_c_compiler(@VEXEROOT, base)
	assert selection.implicit_tcc == ''
	assert selection.c_compiler == 'cc'
	windows_target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	windows_selection := v3_select_c_compiler(@VEXEROOT, V3BundledTccProbeOptions{
		...base
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert windows_selection.implicit_tcc == ''
	// On Windows the platform compiler is resolved to an absolute path, so a child
	// re-exec can find it (v3_platform_c_compiler_command). Without gcc on PATH the
	// helper falls back to the bare name, which keeps the basename check valid.
	assert windows_selection.c_compiler == v3_platform_c_compiler_command('windows')
	assert os.file_name(windows_selection.c_compiler).starts_with('gcc')
}
