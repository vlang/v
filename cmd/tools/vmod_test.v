import os

const vexe = os.quoted_path(@VEXE)

const tfolder = os.join_path(os.vtmp_dir(), 'vmod_test_${os.getpid()}')
const project_free_folder = make_project_free_folder()
const original_dir = os.getwd()

fn has_project_ancestor(path string) bool {
	mut current := os.real_path(path)
	for current != '' {
		if os.is_file(os.join_path(current, 'v.mod')) {
			return true
		}
		parent := os.dir(current)
		if parent == current { break }
		current = parent
	}
	return false
}

fn select_project_free_temp_dir(candidates []string) !string {
	for candidate in candidates {
		if os.is_dir(candidate) && !has_project_ancestor(candidate) {
			return os.real_path(candidate)
		}
	}
	return error('no temporary directory without a v.mod ancestor is available')
}

fn make_project_free_folder() string {
	mut candidates := [os.temp_dir()]
	$if windows {
		candidates << os.getenv('TEMP')
		candidates << os.getenv('TMP')
		if system_root := os.getenv_opt('SystemRoot') {
			candidates << os.join_path(system_root, 'Temp')
		}
	} $else {
		// Linux TMPDIR may point inside a checkout; /tmp supplies the OS fallback.
		candidates << '/tmp'
	}
	root := select_project_free_temp_dir(candidates) or { panic(err) }
	return os.join_path(root, 'vmod_no_project_${os.getpid()}')
}

fn prepare_project_free_folder() ! {
	os.mkdir_all(project_free_folder)!
	os.chdir(project_free_folder)!
}

// write_file creates `path` and its parent folders.
fn write_file(path string, content string) ! {
	os.mkdir_all(os.dir(path))!
	os.write_file(path, content)!
}

// write_module lays out a module with a v.mod and one source file, and returns
// its root. The tests use sibling modules, which is how the compiler resolves a
// module checked out next to the project.
fn write_module(name string, source string) !string {
	root := os.join_path_single(tfolder, name)
	write_file(os.join_path_single(root, 'v.mod'), "Module {\n\tname: '${name}'\n}\n")!
	write_file(os.join_path_single(root, name + '.v'), source)!
	return root
}

fn prepare_fixture() ! {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder)!
	// `app` imports `lib`, which imports `deeper`, so the chain is three deep.
	// `lib` also uses the `as` and `{ }` import forms.
	write_file(os.join_path(tfolder, 'app', 'v.mod'), "Module {\n\tname: 'app'\n}\n")!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\n\nimport lib\n\nfn main() {\n\tprintln(lib.hello())\n}\n')!
	write_module('lib', 'module lib\n\nimport deeper as d\nimport os { getwd }\n\npub fn hello() string {\n\t_ := getwd()\n\treturn d.value()\n}\n')!
	write_module('deeper', "module deeper\n\npub fn value() string {\n\treturn 'deep'\n}\n")!
	write_module('orphan', 'module orphan\n\npub fn unused() int {\n\treturn 1\n}\n')!
	os.chdir(os.join_path(tfolder, 'app'))!
}

fn mod_why(name string) os.Result {
	return os.exec([@VEXE, 'mod', 'why', '${name}'])
}

// test_v_mod_why_prints_the_import_chain is the whole point of the command: the
// path of modules from the project root down to the one asked about.
fn test_v_mod_why_prints_the_import_chain() {
	prepare_fixture()!
	res := mod_why('deeper')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'lib', 'deeper'], res.output
}

// test_v_mod_why_reports_a_direct_dependency without the tail, to show the chain
// is cut at the module that was asked for.
fn test_v_mod_why_reports_a_direct_dependency() {
	prepare_fixture()!
	res := mod_why('lib')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'lib'], res.output
}

// test_v_mod_why_reads_the_as_and_brace_import_forms covers the two shapes that
// carry extra syntax after the module path, which a naive scan would mistake for
// part of the path.
fn test_v_mod_why_reads_the_as_and_brace_import_forms() {
	prepare_fixture()!
	// `deeper` is only reachable through `import deeper as d` in `lib`.
	res := mod_why('deeper')
	assert res.output.contains('deeper'), res.output
	assert !res.output.contains('as'), res.output
}

// test_v_mod_why_reports_the_project_itself: the root module needs nothing to be
// reachable, so it answers with itself.
fn test_v_mod_why_reports_the_project_itself() {
	prepare_fixture()!
	res := mod_why('app')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'app', res.output
}

// test_v_mod_why_separates_unused_from_missing is the distinction that matters:
// an installed module nothing imports is a different problem from one that is
// not installed, and `go mod why` words them differently.
fn test_v_mod_why_separates_unused_from_missing() {
	prepare_fixture()!
	unused := mod_why('orphan')
	assert unused.exit_code == 0, unused.output
	assert unused.output.trim_space() == '(main module does not need module `orphan`)', unused.output
	missing := mod_why('nosuchmodule')
	assert missing.exit_code == 1
	assert missing.output.contains('no module named `nosuchmodule`'), missing.output
}

fn test_v_mod_why_needs_a_project() {
	prepare_project_free_folder()!
	os.rmdir_all(os.join_path(tfolder, 'app')) or {}
	res := os.exec([@VEXE, 'mod', 'why', 'os'])
	assert res.exit_code == 1
	assert res.output.contains('no v.mod found'), res.output
}

fn mod_graph() os.Result {
	return os.exec([@VEXE, 'mod', 'graph', '--imports'])
}

// test_v_mod_graph_prints_the_dependency_graph is the picture `v mod why` answers one
// question about: every module and the modules it imports, indented.
fn test_v_mod_graph_prints_the_dependency_graph() {
	prepare_fixture()!
	res := mod_graph()
	assert res.exit_code == 0, res.output
	lines := res.output.trim_space().split_into_lines()
	assert lines[0] == 'app', res.output
	assert lines.contains('lib'), res.output
	assert lines.contains('  deeper'), res.output
}

// test_v_mod_graph_indents_nested_dependencies: a module imported by an imported
// module is indented further than a direct dependency.
fn test_v_mod_graph_indents_nested_dependencies() {
	prepare_fixture()!
	res := mod_graph()
	assert res.exit_code == 0, res.output
	lines := res.output.trim_space().split_into_lines()
	lib_idx := lines.index('lib')
	deeper_idx := lines.index('  deeper')
	assert deeper_idx > lib_idx, res.output
	assert lines[deeper_idx].starts_with('  '), lines[deeper_idx]
}

// test_v_mod_graph_prints_each_module_once: a diamond must not repeat a subtree.
fn test_v_mod_graph_prints_each_module_once() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport near\nimport far\nfn main() {}\n')!
	write_module('near', 'module near\nimport shared\n')!
	write_module('far', 'module far\nimport shared\n')!
	write_module('shared', 'module shared\n')!
	res := mod_graph()
	assert res.exit_code == 0, res.output
	lines := res.output.trim_space().split_into_lines()
	mut shared_count := 0
	for line in lines {
		if line.trim_space() == 'shared' {
			shared_count++
		}
	}
	assert shared_count == 1, res.output
}

fn test_v_mod_graph_stops_at_import_cycles_and_keeps_other_branches() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport near\nimport far\nfn main() {}\n')!
	write_module('near', 'module near\nimport cycle\n')!
	write_module('cycle', 'module cycle\nimport near\n')!
	write_module('far', 'module far\nimport shared\n')!
	write_module('shared', 'module shared\n')!
	res := mod_graph()
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'near', '  cycle', 'far', '  shared'], res.output
}

// test_v_mod_graph_needs_a_project: like every `v mod` subcommand, it has to run from
// a project folder.
fn test_v_mod_graph_needs_a_project() {
	prepare_project_free_folder()!
	os.rmdir_all(os.join_path(tfolder, 'app')) or {}
	res := os.exec([@VEXE, 'mod', 'graph'])
	assert res.exit_code == 1
	assert res.output.contains('no v.mod found'), res.output
}

// test_v_mod_help_lists_graph: the help text has to name the subcommand.
fn test_v_mod_help_lists_graph() {
	res := os.exec([@VEXE, 'mod'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('graph'), res.output
}

fn test_v_mod_rejects_an_unknown_subcommand() {
	prepare_fixture()!
	res := os.exec([@VEXE, 'mod', 'nosuchsubcommand'])
	assert res.exit_code == 1
	assert res.output.contains('unknown subcommand `nosuchsubcommand`.'), res.output
}

fn test_v_mod_help_lists_the_subcommands() {
	res := os.exec([@VEXE, 'mod'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('why MODULE'), res.output
}

fn testsuite_end() {
	os.chdir(original_dir) or {}
	os.rmdir_all(tfolder) or {}
	os.rmdir_all(project_free_folder) or {}
}

fn test_v_mod_why_preserves_standard_library_import_names() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport os\nfn main() { println(os.getwd()) }\n')!
	res := mod_why('os')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'os'], res.output
}

fn test_v_mod_why_preserves_import_names_when_manifest_names_differ() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'lib', 'v.mod'), "Module { name: 'different_manifest_name' }\n")!
	res := mod_why('deeper')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'lib', 'deeper'], res.output
}

fn test_v_mod_why_resolves_dependencies_from_bare_submodules() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport lib.inner\nfn main() { println(inner.value()) }\n')!
	write_file(os.join_path(tfolder, 'lib', 'inner', 'inner.v'), 'module inner\nimport deeper\npub fn value() string { return deeper.value() }\n')!
	res := mod_why('deeper')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'lib.inner', 'deeper'], res.output
	inner := mod_why('lib.inner')
	assert inner.exit_code == 0, inner.output
	assert inner.output.trim_space().split_into_lines() == ['app', 'lib.inner'], inner.output
}

fn test_v_mod_why_reads_tab_separated_import_tokens() {
	for import_line in ['import\tos', 'import\tos as local_os', 'import\tos { getwd }'] {
		prepare_fixture()!
		call := if import_line.contains('local_os') {
			'local_os.getwd()'
		} else if import_line.contains('{') {
			'getwd()'
		} else {
			'os.getwd()'
		}
		write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\n${import_line}\nfn main() { println(${call}) }\n')!
		checked := os.exec([@VEXE, '-check', '.'])
		assert checked.exit_code == 0, checked.output
		res := mod_why('os')
		assert res.exit_code == 0, res.output
		assert res.output.trim_space().split_into_lines() == ['app', 'os'], res.output
	}
}

fn test_v_mod_why_reads_keyword_root_imports() {
	prepare_fixture()!
	write_module('type', 'module type\npub fn value() string { return "keyword" }\n')!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport type as tp\nfn main() { println(tp.value()) }\n')!
	checked := os.exec([@VEXE, '-check', '.'])
	assert checked.exit_code == 0, checked.output
	res := mod_why('type')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'type'], res.output
}

fn test_v_mod_why_reads_keyword_submodule_imports() {
	prepare_fixture()!
	write_module('pkg', 'module pkg\n')!
	write_file(os.join_path(tfolder, 'pkg', 'type', 'type.v'), 'module type\npub fn value() string { return "keyword" }\n')!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport pkg.type as tp\nfn main() { println(tp.value()) }\n')!
	checked := os.exec([@VEXE, '-check', '.'])
	assert checked.exit_code == 0, checked.output
	res := mod_why('pkg.type')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'pkg.type'], res.output
}

fn test_v_mod_why_ignores_fields_named_import() {
	for declaration in ['struct Example { import string }', 'struct Example {\n\timport string\n}'] {
		prepare_fixture()!
		write_module('string', 'module string\npub fn value() int { return 1 }\n')!
		write_file(os.join_path(tfolder, 'app', 'main.v'), "module main\n${declaration}\nfn main() { println(Example{ import: 'value' }) }\n")!
		checked := os.exec([@VEXE, '-check', '.'])
		assert checked.exit_code == 0, checked.output
		res := mod_why('string')
		assert res.exit_code == 0, res.output
		assert res.output.trim_space() == '(main module does not need module `string`)', res.output
	}
}

fn test_v_mod_why_preserves_imports_in_comptime_declaration_branches() {
	prepare_fixture()!
	write_module('string', 'module string\npub fn value() int { return 1 }\n')!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\n\$if true {\n\timport lib\n\tstruct Example { import string }\n} \$else {\n\timport orphan\n}\nfn main() { println(lib.hello()) }\n')!
	checked := os.exec([@VEXE, '-check', '.'])
	assert checked.exit_code == 0, checked.output
	for dependency in ['lib', 'orphan'] {
		res := mod_why(dependency)
		assert res.exit_code == 0, res.output
		assert res.output.trim_space().split_into_lines() == ['app', dependency], res.output
	}
	unused := mod_why('string')
	assert unused.exit_code == 0, unused.output
	assert unused.output.trim_space() == '(main module does not need module `string`)', unused.output
}

fn test_v_mod_why_selects_the_shortest_import_chain() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport near\nimport far\nfn main() { println(near.value() + far.value()) }\n')!
	write_module('near', 'module near\nimport shared\npub fn value() int { return shared.value() }\n')!
	write_module('far', 'module far\nimport mid\npub fn value() int { return mid.value() }\n')!
	write_module('mid', 'module mid\nimport shared\npub fn value() int { return shared.value() }\n')!
	write_module('shared', 'module shared\npub fn value() int { return 1 }\n')!
	res := mod_why('shared')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'near', 'shared'], res.output
}

fn test_v_mod_why_handles_import_cycles_and_unreachable_modules() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'main.v'), 'module main\nimport near\nimport far\nfn main() {}\n')!
	write_module('near', 'module near\nimport cycle\n')!
	write_module('cycle', 'module cycle\nimport near\n')!
	write_module('far', 'module far\nimport mid\n')!
	write_module('mid', 'module mid\nimport shared\n')!
	write_module('shared', 'module shared\nimport far\n')!
	res := mod_why('shared')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['app', 'far', 'mid', 'shared'], res.output
	unreachable := mod_why('orphan')
	assert unreachable.exit_code == 0, unreachable.output
	assert unreachable.output.trim_space() == '(main module does not need module `orphan`)', unreachable.output
}

fn test_v_mod_graph_lists_manifest_requirements_without_imports() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'v.mod'), "Module { name: 'app' dependencies: ['lib@^1.2', 'ghost@^2'] }\n")!
	write_file(os.join_path(tfolder, 'app', 'lib', 'v.mod'), "Module { name: 'lib' version: '1.3.0' }\n")!
	res := os.exec([@VEXE, 'mod', 'graph'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('app -> lib@1.3.0 (requires ^1.2)'), res.output
	assert res.output.contains('ghost (requires ^2) (not installed)'), res.output
	assert res.output.contains('(not installed)'), res.output
}

fn test_project_free_fixture_skips_a_temporary_directory_inside_a_project() {
	blocked := os.join_path(tfolder, 'project_free_selection')
	nested := os.join_path(blocked, 'temporary', 'nested')
	os.mkdir_all(nested)!
	os.write_file(os.join_path(blocked, 'v.mod'), "Module { name: 'temporary_project' }\n")!
	defer { os.rmdir_all(blocked) or {} }
	assert has_project_ancestor(nested)
	fallback := os.dir(project_free_folder)
	selected := select_project_free_temp_dir([nested, fallback])!
	assert selected == os.real_path(fallback)
	assert !has_project_ancestor(selected)
}
