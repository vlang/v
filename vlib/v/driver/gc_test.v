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
	assert !v3_gc_is_disabled(false, false, host.os, host.arch)
	assert v3_gc_is_disabled(true, false, host.os, host.arch)
	assert v3_gc_is_disabled(false, true, host.os, host.arch)
	assert v3_gc_is_disabled(false, false, cross_os, host.arch)
	assert v3_gc_is_disabled(false, false, host.os, cross_arch)
	assert v3_gc_is_disabled(false, false, 'cross', host.arch)
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
