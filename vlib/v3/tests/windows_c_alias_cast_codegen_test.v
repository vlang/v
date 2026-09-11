import os

const windows_c_alias_vexe = @VEXE
const windows_c_alias_tests_dir = os.dir(@FILE)
const windows_c_alias_v3_dir = os.dir(windows_c_alias_tests_dir)
const windows_c_alias_vlib_dir = os.dir(windows_c_alias_v3_dir)
const windows_c_alias_v3_src = os.join_path(windows_c_alias_v3_dir, 'v3.v')

fn test_windows_c_alias_cast_generates_a_c_cast() {
	root := os.join_path(os.temp_dir(), 'v3_windows_c_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := os.execute('${os.quoted_path(windows_c_alias_vexe)} -old-compiler -gc none -path "${windows_c_alias_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(windows_c_alias_v3_src)}')
	assert build.exit_code == 0, build.output
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import os\nimport v.build_constraint\n\nfn main() {\n\t_ := os.stat('.') or { return }\n\tenvironment := build_constraint.new_environment(['windows'], [])\n\t_ := environment.eval('windows') or { false }\n}\n")!
	c_path := os.join_path(root, 'main.c')
	generate := os.execute('${os.quoted_path(v3_bin)} -silent -no-parallel -os windows -cc tcc -gc none -o ${os.quoted_path(c_path)} ${os.quoted_path(source)}')
	assert generate.exit_code == 0, generate.output
	c_source := os.read_file(c_path)!
	assert c_source.contains('int wmain(int argc, wchar_t** argv) {'), c_source
	assert c_source.contains('u32 mode = (DWORD)(0);'), c_source
	assert !c_source.contains('typedef struct __stat64 __stat64;'), c_source
	assert c_source.contains('typedef struct Optional_Array {'), c_source
	assert !c_source.contains('CreatePipe(&__ref_arg_'), c_source
}
