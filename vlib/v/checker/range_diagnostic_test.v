import os

const range_diagnostic_vexe = os.quoted_path(@VEXE)

fn test_range_folding_does_not_recheck_nested_comptime_if() {
	root := os.join_path(os.vtmp_dir(), 'range_nested_comptime_if_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	source := 'fn main() {\n\tfor _ in 0 .. (\$if f64(2.0) ** f64(2.0) == 4.0 { 2 } \$else { 3 }) {}\n}\n'
	os.write_file(program, source) or { panic(err) }
	result := os.execute('${range_diagnostic_vexe} -w -check ${os.quoted_path(program)}')
	assert result.exit_code == 0, result.output
}

fn test_range_folding_tracks_checked_overloads_by_receiver() {
	root := os.join_path(os.vtmp_dir(), 'range_checked_overload_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	source := 'type CheckedAddend = u8\n\nfn (a CheckedAddend) + (b CheckedAddend) CheckedAddend {\n\treturn CheckedAddend(u8(a) + u8(b))\n}\n\ntype ForwardInvalidAddend = u8\n\nfn main() {\n\tfor _ in 2 .. int(ForwardInvalidAddend(1) + ForwardInvalidAddend(0)) {}\n}\n\nfn (a ForwardInvalidAddend) + (b ForwardInvalidAddend) int {\n\treturn 1\n}\n'
	os.write_file(program, source) or { panic(err) }
	result := os.execute('${range_diagnostic_vexe} -w -check ${os.quoted_path(program)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('operator `+` methods on primitive aliases should return `ForwardInvalidAddend`'), result.output

	assert !result.output.contains('empty range'), result.output
}

fn test_nofloat_range_uses_lowered_operand_semantics() {
	root := os.join_path(os.vtmp_dir(), 'range_nofloat_operands_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	source := 'fn main() {\n\tmut entered := false\n\tfor _ in 0 .. int(f64(2.1) - f64(1.9)) {\n\t\tentered = true\n\t}\n\tassert entered\n}\n'
	os.write_file(program, source) or { panic(err) }
	result := os.execute('${range_diagnostic_vexe} -w -nofloat run ${os.quoted_path(program)}')
	assert result.exit_code == 0, result.output
}

fn test_checked_overflow_range_bound_compiles_and_panics() {
	root := os.join_path(os.vtmp_dir(), 'range_checked_overflow_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'main')
	source := 'fn main() {\n\tfor _ in u8(1) .. u8(255) + u8(1) {}\n}\n'
	os.write_file(program, source) or { panic(err) }
	build_result :=
		os.execute('${range_diagnostic_vexe} -w -check-overflow -o ${os.quoted_path(executable)} ${os.quoted_path(program)}')
	assert build_result.exit_code == 0, build_result.output
	run_result := os.execute(os.quoted_path(executable))
	assert run_result.exit_code != 0, run_result.output
	assert run_result.output.contains('attempt to add with overflow'), run_result.output
}

fn test_ignore_overflow_range_bound_is_folded() {
	root := os.join_path(os.vtmp_dir(), 'range_ignore_overflow_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	source := '@[ignore_overflow]\nfn main() {\n\tfor _ in u8(1) .. u8(255) + u8(1) {}\n}\n'
	os.write_file(program, source) or { panic(err) }
	result :=
		os.execute('${range_diagnostic_vexe} -w -check-overflow -check ${os.quoted_path(program)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('empty range: `1 .. 0` will never execute'), result.output
}

fn test_translated_range_arithmetic_uses_c_promotions() {
	root := os.join_path(os.vtmp_dir(), 'range_translated_arithmetic_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	body := 'fn main() {\n\tmut entered := false\n\tfor _ in u8(1) .. u8(255) + u8(1) {\n\t\tentered = true\n\t\tbreak\n\t}\n\tassert entered\n}\n'
	flag_program := os.join_path(root, 'flag.v')
	os.write_file(flag_program, body) or { panic(err) }
	flag_result :=
		os.execute('${range_diagnostic_vexe} -w -translated run ${os.quoted_path(flag_program)}')
	assert flag_result.exit_code == 0, flag_result.output
	file_program := os.join_path(root, 'file.v')
	os.write_file(file_program, '@[translated]\nmodule main\n\n${body}') or { panic(err) }
	file_result := os.execute('${range_diagnostic_vexe} -w run ${os.quoted_path(file_program)}')
	assert file_result.exit_code == 0, file_result.output
}
