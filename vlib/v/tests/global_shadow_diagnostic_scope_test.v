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

const nested_entry_dir = os.join_path(app_dir, 'cmd', 'tool')

const sibling_modules_dir = os.join_path(tmp_root, 'vmods')

const installed_app_dir = os.join_path(sibling_modules_dir, 'my_package')

const nested_modules_dir = os.join_path(app_dir, '.vmodules')

const private_modules_dir = os.join_path(tmp_root, 'private_modules')

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

// write_project_at lays the app out with its dependency installed under
// `modules_dir`, and the module the project owns shadowing the global or not.
fn write_project_at(project_dir string, modules_dir string, helper_shadows bool) {
	os.rmdir_all(tmp_root) or {}
	write_file(os.join_path(project_dir, 'v.mod'), "Module {\n\tname: 'app'\n}\n")
	write_file(os.join_path(project_dir, 'main.v'), '@[has_globals]\nmodule main\n\nimport shadowdep\nimport helpers\n\n__global (\n\tcounter int\n)\n\nfn main() {\n\tprintln(shadowdep.compute() + helpers.helper())\n}\n')
	helper_local := if helper_shadows { 'counter' } else { 'total' }
	// A module the project owns: not the entry file, so only the directory
	// widening reaches it.
	write_file(os.join_path(project_dir, 'helpers', 'helpers.v'), 'module helpers\n\npub fn helper() int {\n\t${helper_local} := 7\n\treturn ${helper_local}\n}\n')
	// The dependency, resolved through VMODULES.
	write_file(os.join_path(modules_dir, 'shadowdep', 'shadowdep.v'), 'module shadowdep\n\npub fn compute() int {\n\tcounter := 41\n\treturn counter + 1\n}\n')
}

fn write_project(modules_dir string, helper_shadows bool) {
	write_project_at(app_dir, modules_dir, helper_shadows)
}

fn testsuite_end() {
	os.rmdir_all(tmp_root) or {}
}

fn compile_project_with_path(project_dir string, modules_dir string, module_path string) os.Result {
	os.setenv('VMODULES', modules_dir, true)
	out := os.join_path(tmp_root, 'app.c')
	path_option := if module_path.len > 0 {
		' -path ${os.quoted_path(module_path)}'
	} else {
		''
	}
	return os.execute('${os.quoted_path(vexe)} -enable-globals${path_option} -o ${os.quoted_path(out)} ${os.quoted_path(project_dir)}')
}

fn compile_app_with_path(modules_dir string, module_path string) os.Result {
	return compile_project_with_path(app_dir, modules_dir, module_path)
}

fn compile_app(modules_dir string) os.Result {
	return compile_app_with_path(modules_dir, '')
}

fn test_project_module_shadow_is_reported() {
	write_project(sibling_modules_dir, true)
	res := compile_app(sibling_modules_dir)
	assert res.exit_code != 0, res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
}

// Imports resolve from the nearest `v.mod`, even when the explicit entry is a
// nested directory. The same project root must own sibling modules for diagnostics.
fn test_vmod_root_owns_sibling_module_of_nested_entry() {
	write_project(sibling_modules_dir, true)
	main_file := os.join_path(app_dir, 'main.v')
	main_source := os.read_file(main_file) or { panic(err) }
	write_file(os.join_path(nested_entry_dir, 'main.v'), main_source)
	os.rm(main_file) or { panic(err) }
	res := compile_project_with_path(nested_entry_dir, sibling_modules_dir, '')
	assert res.exit_code != 0, res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert !res.output.contains('shadowdep.v'), res.output
}

// Open generic bodies are deferred until a concrete specialization is needed.
// Their source bindings still obey the global-shadow invariant in normal builds.
fn test_reachable_generic_local_shadow_is_reported() {
	os.rmdir_all(tmp_root) or {}
	write_file(os.join_path(app_dir, 'v.mod'), "Module {\n\tname: 'app'\n}\n")
	write_file(os.join_path(app_dir, 'main.v'), '@[has_globals]\nmodule main\n\n__global (\n\tcounter int\n)\n\nfn get[T](x T) T {\n\tcounter := x\n\treturn counter\n}\n\nfn main() {\n\tprintln(get[int](1))\n}\n')
	res := compile_project_with_path(app_dir, sibling_modules_dir, '')
	assert res.exit_code != 0, res.output
	assert res.output.contains('main.v'), res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
}

fn test_inactive_custom_flag_branch_in_generic_does_not_report_shadow() {
	os.rmdir_all(tmp_root) or {}
	write_file(os.join_path(app_dir, 'v.mod'), "Module {\n\tname: 'app'\n}\n")
	write_file(os.join_path(app_dir, 'main.v'), '@[has_globals]\nmodule main\n\n__global (\n\tcounter int\n)\n\nfn get[T](x T) T {\n\t$if shadow_branch ? {\n\t\tcounter := 1\n\t\tprintln(counter)\n\t}\n\treturn x\n}\n\nfn main() {\n\tprintln(get[int](1))\n}\n')
	res := compile_project_with_path(app_dir, sibling_modules_dir, '')
	assert res.exit_code == 0, res.output
	assert !res.output.contains('variable `counter` shadows a global variable'), res.output
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

// An installed root that contains the explicit project root is an ancestor,
// not a reason to exclude the project's own imported modules.
fn test_project_inside_module_root_still_reports_the_project() {
	write_project_at(installed_app_dir, sibling_modules_dir, true)
	res := compile_project_with_path(installed_app_dir, sibling_modules_dir, '')
	assert res.exit_code != 0, res.output
	assert res.output.contains(os.join_path('helpers', 'helpers.v')), res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert !res.output.contains('shadowdep.v'), res.output
}

// An explicit `-path` root can hold project-private modules, so it remains
// eligible for diagnostics even though it participates in module resolution.
fn test_private_path_module_is_reported() {
	write_project(sibling_modules_dir, false)
	write_file(os.join_path(private_modules_dir, 'privatehelper', 'privatehelper.v'), 'module privatehelper\n\npub fn helper() int {\n\tcounter := 7\n\treturn counter\n}\n')
	main_file := os.join_path(app_dir, 'main.v')
	main_source := os.read_file(main_file) or { panic(err) }
	write_file(main_file, main_source.replace('import helpers', 'import helpers\nimport privatehelper').replace('helpers.helper()', 'helpers.helper() + privatehelper.helper()'))
	module_path := '${private_modules_dir}|@vlib|@vmodules'
	res := compile_app_with_path(sibling_modules_dir, module_path)
	assert res.exit_code != 0, res.output
	assert res.output.contains(os.join_path('privatehelper', 'privatehelper.v')), res.output
	assert res.output.contains('variable `counter` shadows a global variable'), res.output
	assert !res.output.contains('shadowdep.v'), res.output
}
