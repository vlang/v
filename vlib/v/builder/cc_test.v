module builder

fn test_c_error_looks_like_cpp_header_with_clang_style_output() {
	clang_output := "error: unknown type name 'namespace'\nerror: expected ';' after top level declarator"
	assert c_error_looks_like_cpp_header(clang_output)
}

fn test_c_error_looks_like_cpp_header_with_source_excerpt() {
	gcc_output := '/usr/include/H5File.h:18: error: \';\' expected (got "H5")\n| namespace H5 {\n'
	assert c_error_looks_like_cpp_header(gcc_output)
}

fn test_c_error_looks_like_cpp_header_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert !c_error_looks_like_cpp_header(c_output)
}

fn test_c_error_missing_library_name_detects_tcc_output() {
	tcc_output := "tcc: error: library 'pq' not found"
	lib_name := c_error_missing_library_name(tcc_output) or { panic(err) }
	assert lib_name == 'pq'
}

fn test_c_error_missing_library_name_detects_macos_ld_output() {
	clang_output := 'ld: library not found for -lpq\nclang: error: linker command failed with exit code 1'
	lib_name := c_error_missing_library_name(clang_output) or { panic(err) }
	assert lib_name == 'pq'
}

fn test_c_error_missing_library_name_detects_gnu_ld_output() {
	gcc_output := '/usr/bin/ld: cannot find -lpq: No such file or directory'
	lib_name := c_error_missing_library_name(gcc_output) or { panic(err) }
	assert lib_name == 'pq'
}

fn test_c_error_missing_library_name_detects_msvc_output() {
	msvc_output := "LINK : fatal error LNK1181: cannot open input file 'pq.lib'"
	lib_name := c_error_missing_library_name(msvc_output) or { panic(err) }
	assert lib_name == 'pq'
}

fn test_c_error_missing_library_name_ignores_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert c_error_missing_library_name(c_output) == none
}
