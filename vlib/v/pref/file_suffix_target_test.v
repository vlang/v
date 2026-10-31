module pref

import os

// A `_<os>.` marker in a file name removes the file from every other target,
// which is the whole mechanism behind `os_windows.c.v` and friends.
fn test_file_has_incompatible_target_suffix_rejects_other_os_markers() {
	windows := target_from('windows', 'amd64') or { panic(err) }
	linux := target_from('linux', 'amd64') or { panic(err) }
	macos := target_from('macos', 'amd64') or { panic(err) }
	assert !file_has_incompatible_target_suffix('os_windows.c.v', windows)
	assert file_has_incompatible_target_suffix('os_windows.c.v', linux)
	assert !file_has_incompatible_target_suffix('os_nix.c.v', linux)
	assert file_has_incompatible_target_suffix('os_nix.c.v', windows)
	assert !file_has_incompatible_target_suffix('os_linux.c.v', linux)
	assert file_has_incompatible_target_suffix('os_linux.c.v', macos)
	// `_darwin.` and `_macos.` name the same system.
	assert !file_has_incompatible_target_suffix('os_darwin.c.v', macos)
	assert file_has_incompatible_target_suffix('os_macos.c.v', linux)
	// An unsuffixed file is never excluded.
	assert !file_has_incompatible_target_suffix('plain.c.v', windows)
	assert !file_has_incompatible_target_suffix('plain.c.v', linux)
	assert file_has_incompatible_target_suffix('os_freebsd.c.v', linux)
	assert !file_has_incompatible_target_suffix('os_bsd.c.v', macos)
	assert file_has_incompatible_target_suffix('os_bsd.c.v', linux)
	assert file_has_incompatible_target_suffix('os_solaris.c.v', linux)
}

fn test_file_has_incompatible_target_suffix_rejects_other_arch_markers() {
	windows := target_from('windows', 'amd64') or { panic(err) }
	linux_arm64 := target_from('linux', 'arm64') or { panic(err) }
	assert file_has_incompatible_target_suffix('a.arm64.v', windows)
	assert !file_has_incompatible_target_suffix('a.arm64.v', linux_arm64)
	assert !file_has_incompatible_target_suffix('a.amd64.v', windows)
	assert file_has_incompatible_target_suffix('a.amd64.v', linux_arm64)
	assert file_has_incompatible_target_suffix('a.aarch64.v', windows)
	assert !file_has_incompatible_target_suffix('a.riscv32.v', target_from('linux', 'rv32') or {
		panic(err)
	})
}

// The two-entry form only reads the host architecture, so on any host it must
// agree with the explicit target for the host's own os.
fn test_file_has_incompatible_os_suffix_matches_the_explicit_host_form() {
	assert file_has_incompatible_os_suffix('os_nix.c.v', 'windows') ==
		file_has_incompatible_target_suffix('os_nix.c.v', target_from('windows', host_arch()) or {
			host_target()
		})
	assert file_has_incompatible_os_suffix('os_linux.c.v', 'windows') ==
		file_has_incompatible_target_suffix('os_linux.c.v', target_from('windows', host_arch()) or {
			host_target()
		})
	assert file_has_incompatible_os_suffix('plain.c.v', 'windows') ==
		file_has_incompatible_target_suffix('plain.c.v', target_from('windows', host_arch()) or {
			host_target()
		})
}

// A platform-qualified test must not be added to a harness for another os, and a
// backend-qualified one only to a harness for that backend.
fn test_is_test_file_for_target_separates_backend_and_platform() {
	host := host_target().os
	assert is_test_file_for_target('plain_test.v', 'c', host)
	assert is_test_file_for_target('plain_test.c.v', 'c', host)
	assert is_test_file_for_target('plain_test.v', 'js', host)
	assert is_test_file_for_target('plain_windows_test.v', 'c', 'windows')
	assert !is_test_file_for_target('plain_windows_test.v', 'c', 'linux')
	assert is_test_file_for_target('plain_linux_test.v', 'c', 'linux')
	assert !is_test_file_for_target('plain_linux_test.v', 'c', 'windows')
	assert is_test_file_for_target('plain_nix_test.v', 'c', 'linux')
	assert !is_test_file_for_target('plain_nix_test.v', 'c', 'windows')
	// The file has to be a test file at all.
	assert !is_test_file_for_target('plain.v', 'c', host)
	assert !is_test_file_for_target('plain_test.vsh', 'c', host)
	assert !is_test_file_for_target('a_test.c.v', 'js', host)
	// NOTE: `_test.js.v` is disabled on every target, not just for other backends.
	assert !is_test_file_for_target('a_test.js.v', 'js', host)
}

fn test_is_test_file_for_platform_reads_a_backend_name_as_a_backend() {
	linux := target_from('linux', 'amd64') or { panic(err) }
	linux_arm64 := target_from('linux', 'arm64') or { panic(err) }
	assert is_test_file_for_platform('a_test.wasm.v', 'wasm', linux)
	assert !is_test_file_for_platform('a_test.wasm.v', 'c', linux)
	assert !is_test_file_for_platform('a_test.c.v', 'js', linux)
	assert is_test_file_for_platform('a_test.c.v', 'c', linux)
	// An architecture name is a C-backend test that only its own arch may run.
	assert is_test_file_for_platform('a_test.arm64.v', 'c', linux_arm64)
	assert !is_test_file_for_platform('a_test.arm64.v', 'c', linux)
	assert is_test_file_for_platform('a_test.v', 'c', linux)
	assert is_test_file_for_platform('a_test.v', 'js', linux)
	assert is_test_file_for_platform('a_test.v', 'wasm', linux)
}

fn test_is_module_search_stop_dir_requires_the_marker_file() {
	root := os.join_path(os.vtmp_dir(), 'pref_stop_dir_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	assert !is_module_search_stop_dir(root)
	assert is_module_search_stop_dir(os.join_path(root, 'missing')) == false
	os.write_file(os.join_path_single(root, module_search_stop_marker), '') or { panic(err) }
	assert is_module_search_stop_dir(root)
}

// `@vlib` and `@vroot` are placeholders, `|` is an alternative separator, and an
// empty specification must not contribute a single empty root.
fn test_expand_module_search_paths_expands_placeholders_and_delimiters() {
	vroot := os.join_path('somewhere', 'v')
	assert expand_module_search_paths('', vroot) == []
	assert expand_module_search_paths('@vlib', vroot) == [os.join_path_single(vroot, 'vlib')]
	// `@vroot` is a textual substitution, so the separator that follows it is kept.
	assert expand_module_search_paths('@vroot/examples', vroot) == [vroot + '/examples']
	assert expand_module_search_paths('@vroot|@vroot/x', vroot) == [vroot, vroot + '/x']
	assert expand_module_search_paths('a|b', vroot) == ['a', 'b']
	$if windows {
		assert expand_module_search_paths('a;b', vroot) == ['a', 'b']
	} $else {
		assert expand_module_search_paths('a;b', vroot) == ['a;b']
	}
	assert expand_module_search_paths('a|@vroot|b', vroot) == ['a', vroot, 'b']
	assert expand_module_search_paths('plain', vroot) == ['plain']
	// `@vmodules` is the global module folders, which this machine does have.
	assert expand_module_search_paths('@vmodules', vroot).len > 0
	// Every expansion is a distinct root.
	assert expand_module_search_paths('a|b', vroot).len == 2
}
