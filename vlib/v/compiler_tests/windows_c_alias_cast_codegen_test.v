import os
import v.tempname

fn test_windows_c_alias_cast_generates_a_c_cast() {
	root := os.join_path(os.vtmp_dir(), 'v3_windows_c_alias_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import os\nimport v.build_constraint\n\nfn main() {\n\t_ := os.stat('.') or { return }\n\tenvironment := build_constraint.new_environment(['windows'], [])\n\t_ := environment.eval('windows') or { false }\n}\n")!
	c_path := os.join_path(root, 'main.c')
	generate := os.exec([@VEXE, '-silent', '-no-parallel', '-os', 'windows', '-cc', 'tcc', '-gc',
		'none', '-o', c_path, source])
	assert generate.exit_code == 0, generate.output
	c_source := os.read_file(c_path)!
	assert c_source.contains('int wmain(int argc, wchar_t** argv) {'), c_source
	assert c_source.contains('DWORD mode = (DWORD)(0);'), c_source
	assert !c_source.contains('typedef struct __stat64 __stat64;'), c_source
	assert c_source.contains('typedef struct __v_result_Array {'), c_source
	assert !c_source.contains('CreatePipe(&__ref_arg_'), c_source
}
