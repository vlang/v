import os

fn test_ownership_signed_integer_parsers_preserve_their_inputs() {
	root := os.join_path(os.vtmp_dir(), 'ownership_signed_integer_parsing_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import strconv

fn main() {
	negative := "-12345".to_owned()
	negative_address := unsafe { usize(negative.str) }
	assert negative.int() == -12345
	assert negative.int() == -12345
	assert strconv.parse_int(negative, 10, 64)! == -12345
	assert negative == "-12345"
	assert unsafe { usize(negative.str) == negative_address }
	positive := "+23456".to_owned()
	positive_address := unsafe { usize(positive.str) }
	assert positive.int() == 23456
	assert strconv.parse_int(positive, 10, 64)! == 23456
	assert positive == "+23456"
	assert unsafe { usize(positive.str) == positive_address }
	unsigned := "34567".to_owned()
	assert unsigned.int() == 34567
	assert strconv.parse_int(unsigned, 10, 64)! == 34567
	assert unsigned == "34567"
	zero := "-0".to_owned()
	assert zero.int() == 0
	assert zero == "-0"
	minimum := "-9223372036854775808".to_owned()
	assert strconv.parse_int(minimum, 10, 64)! == min_i64
	assert minimum == "-9223372036854775808"
	invalid := "-12x".to_owned()
	if _ := strconv.common_parse_int(invalid, 10, 64, true, false) {
		assert false
	}
	assert invalid == "-12x"
	assert invalid.int() == -12
	overflow := "-9223372036854775809".to_owned()
	if _ := strconv.common_parse_int(overflow, 10, 64, true, true) {
		assert false
	}
	assert overflow == "-9223372036854775809"
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert result.exit_code == 0, '${mode}: ${result.output}'
		assert result.output.trim_space() == 'ok', result.output
	}
}
