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

fn test_c_output_suggests_missing_typedef_for_c_struct_with_issue_19050_output() {
	c_output := [
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:12966:17: error: incomplete result type 'struct string_c' in function definition",
		'struct string_c main__convert(string s) {',
		'                ^',
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:1962:8: note: forward declaration of 'struct string_c'",
		'struct string_c main__convert(string s);',
		'       ^',
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:12967:25: error: variable has incomplete type 'struct string_c'",
		'        struct string_c _t1 = ((struct string_c){.content = s.str,.len = ((u32)(s.len)),});',
		'                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
	].join('\n')
	assert c_output_suggests_missing_typedef_for_c_struct(c_output, {
		'string_c': true
	}) == 'string_c'
}

fn test_c_output_suggests_missing_typedef_for_c_struct_requires_matching_redeclaration() {
	c_output := [
		"/tmp/v_501/c_struct.tmp.c:1:1: error: incomplete result type 'struct string_c' in function definition",
		"/tmp/v_501/c_struct.tmp.c:2:1: note: forward declaration of 'struct string_c'",
	].join('\n')
	assert c_output_suggests_missing_typedef_for_c_struct(c_output, {
		'other_c_struct': true
	}) == ''
}
