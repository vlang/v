module driver

import os
import v.pref

fn test_native_prefix_is_target_checked_and_excluded_from_hosted_abis() ! {
	root := os.join_path(os.vtmp_dir(), 'vlibc_prefix_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	target := pref.host_target()
	if target.os != 'linux' { return }
	prefix := os.join_path(root, 'vlibc', '${target.os}-${target.arch}-shared')
	os.mkdir_all(os.join_path(prefix, 'lib', 'pkgconfig'))!
	os.mkdir_all(os.join_path(prefix, 'include'))!
	assert v3_freestanding_native_prefix(target, ['freestanding'], 'c', false, false, root, '')! == ''
	marker := os.join_path(prefix, 'vlibc-target')
	os.write_file(marker, 'vlibc-native-v1\n${target.os}\n${target.arch}\n')!
	assert v3_freestanding_native_prefix(target, ['freestanding'], 'c', false, false, root, '')! == os.real_path(prefix)
	for defines in [[]string{}, ['freestanding', 'v_bare_no_startup'], ['freestanding', 'v_bare_libc']] {
		assert v3_freestanding_native_prefix(target, defines, 'c', false, false, root, prefix)! == ''
	}
	assert v3_freestanding_native_prefix(target, ['freestanding'], 'c', true, false, root, prefix)! == ''
	assert v3_freestanding_native_prefix(target, ['freestanding'], 'c', false, true, root, prefix)! == ''
	os.write_file(marker, 'vlibc-native-v1\nwrong\ntarget\n')!
	if _ := v3_freestanding_native_prefix(target, ['freestanding'], 'c', false, false, root, prefix) {
		assert false, 'a wrong-target prefix must fail before passing native dependencies to C'
	}
	assert v3_freestanding_pkgconfig_path(prefix, 'existing') ==
		[os.join_path(prefix, 'lib', 'pkgconfig'), os.join_path(prefix, 'share', 'pkgconfig'),
			'existing'].join(os.path_delimiter)
}

fn test_dynamic_runtime_does_not_discover_static_only_prefix() ! {
	target := pref.host_target()
	if target.os != 'linux' { return }
	root := os.join_path(os.vtmp_dir(), 'vlibc_static_prefix_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	prefix := os.join_path(root, 'vlibc', '${target.os}-${target.arch}-static')
	os.mkdir_all(os.join_path(prefix, 'lib'))!
	os.mkdir_all(os.join_path(prefix, 'include'))!
	os.write_file(os.join_path(prefix, 'vlibc-target'), 'vlibc-native-v1\n${target.os}\n${target.arch}\n')!
	assert v3_freestanding_native_prefix(target, ['freestanding'], 'c', false, false, root, '')! == ''
	assert v3_freestanding_native_prefix(target, ['freestanding', 'v_bare_static'], 'c', false, false, root, '')! == os.real_path(prefix)
}
