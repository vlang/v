import os

const vexe = os.quoted_path(@VEXE)

const tfolder = os.join_path(os.vtmp_dir(), 'vmod_test')

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
	os.chdir(os.vtmp_dir())!
	os.rmdir_all(os.join_path(tfolder, 'app')) or {}
	res := os.exec([@VEXE, 'mod', 'why', 'os'])
	assert res.exit_code == 1
	assert res.output.contains('no v.mod found'), res.output
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
	os.rmdir_all(tfolder) or {}
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
