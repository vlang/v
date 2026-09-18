module driver

import os

// test_nearest_vroot_for_path_does_not_fall_back_to_cwd pins the regression from
// https://github.com/vlang/v/issues/28583: the parent walk must stop at a
// filesystem root instead of continuing through the relative `.` that `os.dir`
// returns for a path without a separator (`os.dir('S:') == '.'` on Windows).
// A walk that continued from `.` probed `./vlib/builtin`, which matches exactly
// when V is run from the V root, so every input outside a V checkout resolved
// `vroot` to `.` and every derived tcc/include path became relative.
fn test_nearest_vroot_for_path_does_not_fall_back_to_cwd() {
	outside := os.join_path(os.vtmp_dir(), 'v_vroot_outside_${os.getpid()}', 'a', 'b')
	os.mkdir_all(outside) or { panic(err) }
	defer {
		os.rmdir_all(os.dir(os.dir(outside))) or {}
	}
	cwd := os.getwd()
	os.chdir(@VMODROOT) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	input := os.join_path(outside, 'hello.v')
	if root := nearest_vroot_for_path(input) {
		assert root != '.'
		assert os.is_abs_path(root)
		assert is_valid_vroot(root)
	}
}

// test_nearest_vroot_for_path_finds_the_enclosing_checkout keeps the ordinary
// case working: an input inside a V checkout resolves to that checkout.
fn test_nearest_vroot_for_path_finds_the_enclosing_checkout() {
	vroot := @VMODROOT
	input := os.join_path(vroot, 'vlib', 'v', 'driver', 'driver.v')
	root := nearest_vroot_for_path(input) or { panic('vroot not detected for ${input}') }
	assert os.real_path(root) == os.real_path(vroot)
}
