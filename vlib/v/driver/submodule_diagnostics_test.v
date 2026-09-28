module driver

import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_diagnostic_files_include_project_submodules() {
	root := os.join_path(os.vtmp_dir(), 'v3_submodule_diagnostic_files_${os.getpid()}')
	project := os.join_path(root, 'project')
	private_root := os.join_path(root, 'private')
	dependency_root := os.join_path(project, '.vmodules')
	vlib_root := os.join_path(root, 'vlib')
	entry := os.join_path(project, 'cmd', 'main.v')
	direct := os.join_path(project, 'foo', 'foo.v')
	nested := os.join_path(project, 'foo', 'bar', 'bar.v')
	private_file := os.join_path(private_root, 'helper', 'helper.v')
	dependency := os.join_path(dependency_root, 'dependency', 'dependency.v')
	builtin := os.join_path(vlib_root, 'builtin', 'builtin.v')
	stdlib := os.join_path(vlib_root, 'os', 'os.v')
	outside := os.join_path(root, 'project_other', 'other.v')
	paths := [entry, direct, nested, private_file, dependency, builtin, stdlib, outside]
	for path in paths {
		os.mkdir_all(os.dir(path))!
		os.write_file(path, '')!
	}
	defer {
		os.rmdir_all(root) or {}
	}

	mut a := flat.FlatAst.new()
	a.add_val(.file, builtin)
	a.user_code_start = a.nodes.len
	for path in paths {
		if path != builtin {
			a.add_val(.file, path)
		}
	}
	mut tc := types.TypeChecker.new(&a)
	tc.shadow_diagnostic_root = os.real_path(project)
	tc.shadow_explicit_roots = [os.real_path(private_root)]
	tc.shadow_dependency_roots = [os.real_path(dependency_root), os.real_path(vlib_root)]
	set_diagnostic_files(mut tc, [entry])

	for path in [entry, direct, nested, private_file] {
		assert path in tc.diagnostic_files, path
	}
	for path in [dependency, builtin, stdlib, outside] {
		assert path !in tc.diagnostic_files, path
	}
	// Explicit inputs still take precedence over dependency exclusions.
	set_diagnostic_files(mut tc, [dependency])
	assert dependency in tc.diagnostic_files
}

fn test_diagnostic_files_preserve_checker_fixture_selection() {
	root := os.join_path(os.vtmp_dir(), 'v3_submodule_fixture_selection_${os.getpid()}')
	entry := os.join_path(root, 'main.vv')
	imported := os.join_path(root, 'foo', 'foo.v')
	mut a := flat.FlatAst.new()
	a.add_val(.file, entry)
	a.add_val(.file, imported)
	mut tc := types.TypeChecker.new(&a)
	tc.shadow_diagnostic_root = root
	tc.checker_fixture_mode = true
	set_diagnostic_files(mut tc, [entry])
	assert entry in tc.diagnostic_files
	assert imported !in tc.diagnostic_files
}

fn submodule_source_errors(name string, source string) []types.TypeError {
	root := os.join_path(os.vtmp_dir(), 'v3_submodule_${name}_${os.getpid()}')
	path := os.join_path(root, 'foo', 'foo.v')
	entry := os.join_path(root, 'main.v')
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
	os.write_file(entry, 'fn main() {}\n') or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.shadow_diagnostic_root = os.real_path(root)
	tc.diagnose_unknown_calls = true
	tc.no_main = true
	// Only the entry file is selected explicitly, as in an ordinary build.
	set_diagnostic_files(mut tc, [entry])
	tc.collect(a)
	tc.check_semantics()
	return tc.errors.clone()
}

fn test_submodule_unknown_struct_field_type_is_reported() {
	errors := submodule_source_errors('unknown_type', 'module foo\n\npub struct Foo {\npub mut:\n\tshould_error ThisIsNotARealStruct\n}\n')
	assert errors.any(it.msg.contains('unknown type')
		&& it.msg.contains('ThisIsNotARealStruct')), errors.str()
	assert errors.any(it.file.ends_with(os.join_path('foo', 'foo.v'))), errors.str()
}

fn test_submodule_invalid_enum_default_is_reported() {
	errors := submodule_source_errors('enum_default', 'module foo\n\npub struct Foo {\npub mut:\n\te MyEnum = .maybe\n}\n\npub enum MyEnum {\n\tyes\n\tno\n}\n')
	assert errors.any(it.msg.contains('unknown enum field `maybe`')), errors.str()
}

fn test_submodule_function_body_is_checked() {
	errors := submodule_source_errors('function_body', "module foo\n\npub fn value() int {\n\treturn 'not an integer'\n}\n")
	assert errors.any(it.kind == .return_mismatch), errors.str()
}

fn test_submodule_valid_generic_field_and_enum_default() {
	errors := submodule_source_errors('valid', 'module foo\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n\npub struct Foo {\npub:\n\tbox Box[int]\n\te MyEnum = .yes\n}\n\npub enum MyEnum {\n\tyes\n\tno\n}\n')
	assert errors.len == 0, errors.str()
}
