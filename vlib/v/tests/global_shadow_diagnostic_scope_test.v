// A local that shadows a global is an error, but only in code the project
// owns. A third-party module under VMODULES declares its locals without any
// knowledge of the globals the application happens to define, so blaming it
// would stop an application from building over a name its author cannot see.
import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = os.getenv('VEXE')

const tmp_root = os.join_path(os.vtmp_dir(), 'v_global_shadow_scope')

const app_dir = os.join_path(tmp_root, 'app')

const vmodules_dir = os.join_path(tmp_root, 'vmods')

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn testsuite_begin() {
	os.rmdir_all(tmp_root) or {}
	write_file(os.join_path(app_dir, 'v.mod'), "Module {\n\tname: 'app'\n}\n")
	write_file(os.join_path(app_dir, 'main.v'), '@[has_globals]\nmodule main\n\nimport shadowdep\nimport helpers\n\n__global (\n\tcounter int\n)\n\nfn main() {\n\tprintln(shadowdep.compute() + helpers.helper())\n}\n')
	// A module the project owns: not the entry file, so only the directory
	// widening reaches it.
	write_file(os.join_path(app_dir, 'helpers', 'helpers.v'), 'module helpers\n\npub fn helper() int {\n\tcounter := 7\n\treturn counter\n}\n')
	// A dependency, resolved through VMODULES.
	write_file(os.join_path(vmodules_dir, 'shadowdep', 'shadowdep.v'), 'module shadowdep\n\npub fn compute() int {\n\tcounter := 41\n\treturn counter + 1\n}\n')
}

fn testsuite_end() {
	os.rmdir_all(tmp_root) or {}
}

fn compile_app() os.Result {
	os.setenv('VMODULES', vmodules_dir, true)
	out := os.join_path(tmp_root, 'app.c')
	return os.execute('${os.quoted_path(vexe)} -enable-globals -o ${os.quoted_path(out)} ${os.quoted_path(app_dir)}')
}

fn test_project_module_shadow_is_reported() {
	res := compile_app()
	assert res.exit_code != 0, res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
}

fn test_dependency_shadow_is_not_reported() {
	res := compile_app()
	assert !res.output.contains('shadowdep.v'), res.output
}

fn test_dependency_shadow_alone_still_builds() {
	// With the project's own shadow gone, the dependency's local must not keep
	// the application from compiling.
	write_file(os.join_path(app_dir, 'helpers', 'helpers.v'), 'module helpers\n\npub fn helper() int {\n\ttotal := 7\n\treturn total\n}\n')
	res := compile_app()
	assert res.exit_code == 0, res.output
	assert !res.output.contains('shadows a global variable'), res.output
}
