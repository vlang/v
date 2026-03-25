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

fn test_detect_cc_from_version_output_detects_clang() {
	clang_output := 'Apple clang version 17.0.0 (clang-1700.6.3.2)\nTarget: arm64-apple-darwin25.2.0'
	assert detect_cc_from_version_output(clang_output) == .clang
}

fn test_detect_cc_from_version_output_detects_modern_gcc_output() {
	gcc_output := 'cc (GCC) 14.3.1 20251022 (Red Hat 14.3.1-4)\nCopyright (C) 2025 Free Software Foundation, Inc.'
	assert detect_cc_from_version_output(gcc_output) == .gcc
}

fn test_detect_cc_from_version_output_detects_distro_gcc_alias_output() {
	gcc_output := 'cc (Debian 12.2.0-14) 12.2.0\nCopyright (C) 2022 Free Software Foundation, Inc.'
	assert detect_cc_from_version_output(gcc_output) == .gcc
}

fn test_detect_cc_from_version_output_keeps_unknown_output_unknown() {
	unknown_output := 'zig 0.14.0-dev.1152+abc123'
	assert detect_cc_from_version_output(unknown_output) == .unknown
}
