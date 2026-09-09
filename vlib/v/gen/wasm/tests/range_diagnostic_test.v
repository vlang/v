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
