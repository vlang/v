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

fn test_defines_others_libs_splits_bare_windows_import_lib_names() {
	_, _, libs := [
		CFlag{
			value: 'Version.lib Advapi32.lib Shell32.lib'
		},
	].defines_others_libs()
	assert libs == ['-lVersion', '-lAdvapi32', '-lShell32']
}

fn test_defines_others_libs_keeps_direct_windows_import_lib_paths() {
	direct_path := 'C:/Program Files/Windows Kits/10/Lib/Version.lib'
	_, _, libs := [
		CFlag{
			value: direct_path
		},
	].defines_others_libs()
	assert libs == ['"${direct_path}"']
}

fn test_windows_import_lib_link_args_preserves_quoted_paths_and_converts_mixed_case_names() {
	direct_path := 'C:/Program Files/Windows Kits/10/Lib/Version.LiB'
	args := CFlag{
		value: '"${direct_path}" Version.lib Advapi32.LIB'
	}.windows_import_lib_link_args()
	assert args == ['"${direct_path}"', '-lVersion', '-lAdvapi32']
}

fn test_is_c_source_operand_recognizes_existing_translation_units() {
	test_root := os.join_path(os.vtmp_dir(), 'v_cflag_source_operands')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	for name in ['first.c', 'second.CPP', 'third.cc', 'fourth.cxx', 'fifth.S'] {
		path := os.join_path(test_root, name)
		os.write_file(path, '') or { panic(err) }
		assert is_c_source_operand(path)
		assert is_c_source_operand('"${path}"')
		assert is_c_source_operand("'${path}'")
	}
}

fn test_is_c_source_operand_ignores_everything_that_is_not_a_source_file() {
	test_root := os.join_path(os.vtmp_dir(), 'v_cflag_non_source_operands')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	for name in ['header.h', 'prebuilt.o', 'prebuilt.a'] {
		path := os.join_path(test_root, name)
		os.write_file(path, '') or { panic(err) }
		assert !is_c_source_operand(path)
	}
	assert !is_c_source_operand('-DFOO=1')
	assert !is_c_source_operand('-Wall')
	assert !is_c_source_operand(os.join_path(test_root, 'not_written_out.c'))
}
