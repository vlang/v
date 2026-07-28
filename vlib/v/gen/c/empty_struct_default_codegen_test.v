import os

const empty_struct_default_vexe = os.quoted_path(@VEXE)

fn test_empty_struct_defaults_use_msvc_compatible_initializer() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'empty_struct_default_codegen_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path,
		'import x.json2\n\nfn main() {\n\tparsed := json2.decode[json2.Any](\'{"a":1}\') or { return }\n\tprintln(parsed)\n}\n')!
	result := os.execute('${empty_struct_default_vexe} -o - ${os.quoted_path(source_path)}')
	assert result.exit_code == 0, result.output
	assert result.output.contains('(json2__Null){E_STRUCT}'), result.output
	assert !result.output.contains('(json2__Null){}'), result.output
}
