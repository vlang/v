import os

fn test_wasm_range_diagnostic_uses_backend_semantics() {
	vexe := os.quoted_path(@VEXE)
	work_dir := os.join_path(os.vtmp_dir(), 'wasm_range_diagnostic_test')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}

	source_path := os.join_path(work_dir, 'main.v')
	output_path := os.join_path(work_dir, 'main.wasm')
	os.write_file(source_path, 'fn main() {\n\tfor _ in u8(250) .. -1 {\n\t\tbreak\n\t}\n}\n')!

	res :=
		os.execute('${vexe} -b wasm -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
	assert os.exists(output_path)
}

fn test_wasm_same_type_arithmetic_range_compiles() {
	vexe := os.quoted_path(@VEXE)
	work_dir := os.join_path(os.vtmp_dir(), 'wasm_arithmetic_range_${os.getpid()}')
	os.rmdir_all(work_dir) or {}
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	program := os.join_path(work_dir, 'main.v')
	output := os.join_path(work_dir, 'main.wasm')
	source := 'fn main() {\n\tfor _ in u8(200) .. u8(255) + u8(101) {\n\t\tbreak\n\t}\n}\n'
	os.write_file(program, source) or { panic(err) }
	result := os.execute('${vexe} -b wasm -o ${os.quoted_path(output)} ${os.quoted_path(program)}')
	assert result.exit_code == 0, result.output
	assert os.exists(output)
}

fn test_wasm_wide_literal_range_compiles() {
	vexe := os.quoted_path(@VEXE)
	work_dir := os.join_path(os.vtmp_dir(), 'wasm_wide_literal_range_${os.getpid()}')
	os.rmdir_all(work_dir) or {}
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	program := os.join_path(work_dir, 'main.v')
	output := os.join_path(work_dir, 'main.wasm')
	os.write_file(program, 'fn main() {\n\tfor _ in -4294967295 .. 0 { break }\n}\n') or {
		panic(err)
	}
	result := os.execute('${vexe} -b wasm -o ${os.quoted_path(output)} ${os.quoted_path(program)}')
	assert result.exit_code == 0, result.output
	assert os.exists(output)
}

fn test_wasm_literal_empty_range_is_rejected() {
	vexe := os.quoted_path(@VEXE)
	work_dir := os.join_path(os.vtmp_dir(), 'wasm_literal_empty_range_${os.getpid()}')
	os.rmdir_all(work_dir) or {}
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	program := os.join_path(work_dir, 'main.v')
	output := os.join_path(work_dir, 'main.wasm')
	os.write_file(program, 'fn main() {\n\tfor _ in 4 .. 2 {}\n}\n') or { panic(err) }
	result := os.execute('${vexe} -b wasm -o ${os.quoted_path(output)} ${os.quoted_path(program)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('empty range: `4 .. 2` will never execute'), result.output
}
