import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_is_executable_windows_tests')

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_is_executable_decides_by_extension_on_windows() {
	$if !windows {
		return
	}
	eprintln(@FN)
	for ext in ['exe', 'com', 'bat', 'cmd', 'EXE', 'Cmd'] {
		fpath := os.join_path(tfolder, 'tool.${ext}')
		os.write_file(fpath, 'x')!
		assert os.is_executable(fpath), 'tool.${ext} must be executable'
		assert os.is_executable(fpath.replace('\\', '/')), 'tool.${ext} must be executable with forward slashes'
	}
	for ext in ['txt', 'ps1', 'vbs', 'js', 'msi', 'dll'] {
		fpath := os.join_path(tfolder, 'tool.${ext}')
		os.write_file(fpath, 'x')!
		assert !os.is_executable(fpath), 'tool.${ext} must not be executable'
	}
	no_ext := os.join_path(tfolder, 'tool')
	os.write_file(no_ext, 'x')!
	assert !os.is_executable(no_ext), 'a file without an extension must not be executable'
}

fn test_is_executable_requires_the_file_to_exist_on_windows() {
	$if !windows {
		return
	}
	eprintln(@FN)
	assert !os.is_executable(os.join_path(tfolder, 'missing.exe'))
	assert !os.is_executable(os.join_path(tfolder, 'missing.bat'))
	assert !os.is_executable(os.join_path(tfolder, 'missing'))
	assert !os.is_executable(os.join_path(tfolder, 'missing_dir', 'tool.exe'))
}

fn test_is_executable_follows_a_link_without_an_extension_on_windows() {
	$if !windows {
		return
	}
	eprintln(@FN)
	target := os.join_path(tfolder, 'linked_tool.exe')
	os.write_file(target, 'x')!
	link := os.join_path(tfolder, 'linked_tool')
	os.symlink(target, link) or {
		eprintln('> skipping ${@FN}: creating a symlink needs privileges here (${err})')
		return
	}
	assert os.is_executable(link), 'a link without an extension to a .exe must be executable'
}
