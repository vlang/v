// A local that shadows a global is an error, but only in code the project
// owns. An installed module declares its locals without any knowledge of the
// globals the application happens to define, so blaming it would stop an
// application from building over a name its author cannot see.
//
// Ownership is decided by path, which makes where the module root sits matter:
// it is often a sibling of the project, but an isolated or reproducible build
// puts it inside, as `$PWD/.vmodules`. Both are covered here, because plain
// containment reads the nested one as project code.
import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = os.getenv('VEXE')

const tmp_root = os.join_path(os.vtmp_dir(), 'v_global_shadow_scope')

const app_dir = os.join_path(tmp_root, 'app')

const sibling_modules_dir = os.join_path(tmp_root, 'vmods')

const nested_modules_dir = os.join_path(app_dir, '.vmodules')

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

// write_project lays the app out with its dependency installed under
// `modules_dir`, and the module the project owns shadowing the global or not.
fn write_project(modules_dir string, helper_shadows bool) {
	os.rmdir_all(tmp_root) or {}
	write_file(os.join_path(app_dir, 'v.mod'), "Module {\n\tname: 'app'\n}\n")
	write_file(os.join_path(app_dir, 'main.v'), '@[has_globals]\nmodule main\n\nimport shadowdep\nimport helpers\n\n__global (\n\tcounter int\n)\n\nfn main() {\n\tprintln(shadowdep.compute() + helpers.helper())\n}\n')
	helper_local := if helper_shadows { 'counter' } else { 'total' }
	// A module the project owns: not the entry file, so only the directory
	// widening reaches it.
	write_file(os.join_path(app_dir, 'helpers', 'helpers.v'), 'module helpers\n\npub fn helper() int {\n\t${helper_local} := 7\n\treturn ${helper_local}\n}\n')
	// The dependency, resolved through VMODULES.
	write_file(os.join_path(modules_dir, 'shadowdep', 'shadowdep.v'), 'module shadowdep\n\npub fn compute() int {\n\tcounter := 41\n\treturn counter + 1\n}\n')
}

fn testsuite_end() {
	os.rmdir_all(tmp_root) or {}
}

fn compile_app(modules_dir string) os.Result {
	os.setenv('VMODULES', modules_dir, true)
	out := os.join_path(tmp_root, 'app.c')
	return os.execute('${os.quoted_path(vexe)} -enable-globals -o ${os.quoted_path(out)} ${os.quoted_path(app_dir)}')
}

fn test_project_module_shadow_is_reported() {
	write_project(sibling_modules_dir, true)
	res := compile_app(sibling_modules_dir)
	assert res.exit_code != 0, res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
}

fn test_dependency_shadow_is_not_reported() {
	write_project(sibling_modules_dir, true)
	res := compile_app(sibling_modules_dir)
	assert !res.output.contains('shadowdep.v'), res.output
}

fn test_dependency_shadow_alone_still_builds() {
	write_project(sibling_modules_dir, false)
	res := compile_app(sibling_modules_dir)
	assert res.exit_code == 0, res.output
	assert !res.output.contains('shadows a global variable'), res.output
}

// The module root inside the project root is the case plain containment gets
// wrong: the dependency passes the prefix check and is read as project code.
fn test_nested_module_root_dependency_is_not_reported() {
	write_project(nested_modules_dir, false)
	res := compile_app(nested_modules_dir)
	assert res.exit_code == 0, res.output
	assert !res.output.contains('shadows a global variable'), res.output
	assert !res.output.contains('shadowdep.v'), res.output
}

// Excluding the nested root must not take the project's own modules with it.
fn test_nested_module_root_still_reports_the_project() {
	write_project(nested_modules_dir, true)
	res := compile_app(nested_modules_dir)
	assert res.exit_code != 0, res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
	assert !res.output.contains('shadowdep.v'), res.output
}
