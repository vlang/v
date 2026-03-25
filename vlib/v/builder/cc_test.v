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

fn test_c_error_missing_libatomic_marker_with_tcc_output() {
	c_output := "/tmp/v/vdoc.tmp.c:24184: warning: assignment makes pointer from integer without a cast\ntcc: error: library 'atomic' not found\n"
	assert c_error_missing_libatomic_marker(c_output) == "library 'atomic' not found"
	assert c_error_looks_like_missing_libatomic(c_output)
}

fn test_c_error_missing_libatomic_marker_with_ld_output() {
	c_output := '/usr/bin/ld: cannot find -latomic\ncollect2: error: ld returned 1 exit status\n'
	assert c_error_missing_libatomic_marker(c_output) == 'cannot find -latomic'
	assert c_error_looks_like_missing_libatomic(c_output)
}

fn test_c_error_missing_libatomic_marker_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert c_error_missing_libatomic_marker(c_output) == ''
	assert !c_error_looks_like_missing_libatomic(c_output)
}
