import os

const js_global_export_test_vexe = @VEXE
const js_global_export_test_vroot = @VMODROOT
const js_global_export_test_dir = os.join_path(os.vtmp_dir(), '_js_global_export_test')
const js_global_export_test_source = [
	'@[has_globals]',
	'module main',
	'',
	"@[export: 'TIC']",
	'__global tic = fn (a int) int {',
	'\treturn a + 1',
	'}',
	'',
	'fn main() {',
	'\tassert tic(2) == 3',
	'}',
].join_lines()

fn testsuite_end() {
	os.rmdir_all(js_global_export_test_dir) or {}
}

fn test_js_backend_supports_exported_global_aliases() {
	os.chdir(js_global_export_test_vroot) or { panic(err) }
	os.mkdir_all(js_global_export_test_dir) or { panic(err) }
	source_path := os.join_path(js_global_export_test_dir, 'global_export.v')
	js_path := os.join_path(js_global_export_test_dir, 'global_export.js')
	os.write_file(source_path, js_global_export_test_source) or { panic(err) }
	compile_res :=
		os.execute('${os.quoted_path(js_global_export_test_vexe)} -b js -w -o ${os.quoted_path(js_path)} ${os.quoted_path(source_path)}')
	if compile_res.exit_code != 0 {
		panic(compile_res.output)
	}
	generated := os.read_file(js_path) or { panic(err) }
	assert generated.contains(r'Object.defineProperty($global,"tic", {')
	assert generated.contains('Object.defineProperty(globalThis,"TIC", {')
	assert generated.contains(r'return $global["tic"];')
	assert generated.contains(r'$global["tic"] = value;')
	node_res := os.execute('node --version')
	if node_res.exit_code == 0 {
		run_res := os.execute('node ${os.quoted_path(js_path)}')
		if run_res.exit_code != 0 {
			panic(run_res.output)
		}
	}
}
