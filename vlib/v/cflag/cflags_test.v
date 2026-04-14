module cflag

import os

fn test_format_keeps_include_and_library_paths_with_spaces_in_one_argument() {
	test_root := os.join_path(os.vtmp_dir(), 'v_cflag_spaces')
	include_dir := os.join_path(test_root, 'include dir')
	library_dir := os.join_path(test_root, 'library dir')
	os.mkdir_all(include_dir) or { panic(err) }
	os.mkdir_all(library_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert CFlag{
		name:  '-I'
		value: include_dir
	}.format() or { panic(err) } == '-I"${os.real_path(include_dir)}"'
	assert CFlag{
		name:  '-L'
		value: library_dir
	}.format() or { panic(err) } == '-L"${os.real_path(library_dir)}"'
}
