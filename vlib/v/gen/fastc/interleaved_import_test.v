module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_import_scan_bound_ignores_longer_identifiers() {
	assert fastc_source_last_word_index('', 'import') == -1
	assert fastc_source_last_word_index('imp', 'import') == -1
	assert fastc_source_last_word_index('imported imports _import import2', 'import') == -1
	assert fastc_source_last_word_index('import os\nfn imported() { imports := 1 }', 'import') == 0
	assert fastc_source_last_word_index('import os\nimport strings\nfn imported() {}', 'import') == 10
	// Comments and strings remain conservative candidates for the lexer.
	assert fastc_source_last_word_index('// import absent', 'import') == 3
	assert fastc_source_last_word_index('"import absent"', 'import') == 1
	prefs := pref.new_preferences()
	header := fastc_scan_source_header('module main\nimport os\nconst imported = 1\nimport strings\nfn imports() {}\n', 'import_word_bound.v', prefs) or { panic(err) }
	assert header.import_order == ['os', 'strings']
}

fn test_fastc_imports_after_declarations_keep_source_order() {
	prefs := pref.new_preferences()
	header := fastc_scan_source_header(r'module main
import strings
const marker = 1
import (
	v.token as tok
	v.flat { NodeId }
)
struct Holder { value int }
fn ignored() string { return "import absent" }
import os
// import absent
', 'interleaved_imports.v', prefs) or { panic(err) }
	assert header.import_order == ['strings', 'v.token', 'v.flat', 'os']
	assert header.imports['tok'] == 'v.token'
	assert header.imports[fastc_selective_import_key('NodeId')] == 'v.flat'
	assert header.imports['os'] == 'os'
	assert 'absent' !in header.imports
}

fn test_fastc_import_after_constant_resolves_qualified_return_type() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interleaved_import_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'positions')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'coordinates')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "interleaved_import" }') or { panic(err) }
	os.write_file(os.join_path(root, 'coordinates', 'coordinates.v'), r'module coordinates
pub struct Position { pub: value int }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import positions
fn main() { assert positions.location().value == 42; println("interleaved imports work") }
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		// The C checker diagnoses interleaved imports in user fixtures; keep
		// its oracle equivalent while exercising the compiler-source order in FastC.
		declarations := if backend == 'fastc' {
			'const marker = 42\nimport coordinates\n'
		} else {
			'import coordinates\nconst marker = 42\n'
		}
		os.write_file(os.join_path(root, 'positions', 'positions.v'), 'module positions\n' +
			declarations + 'pub fn location() coordinates.Position { return coordinates.Position{value: marker} }\n') or { panic(err) }
		executable := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o', executable,
			root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == 'interleaved imports work', '${backend}: ${run.output}'
	}
}

fn test_fastc_selected_functions_keep_each_files_import_context() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_selected_functions_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'beta')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "selected_functions" }') or { panic(err) }
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), r'module alpha
pub struct Value {
pub:
	number int
}
pub fn label() string { return "alpha" }
pub fn make_value(number int) Value { return Value{number: number} }
pub fn describe(value &Value) string { return "number=${value.number}" }
fn hidden() int { return 99 }
') or { panic(err) }
	os.write_file(os.join_path(root, 'beta', 'beta.v'), r'module beta
pub fn label() string { return "beta" }
') or { panic(err) }
	os.write_file(os.join_path(root, 'a.v'), r'module main
import alpha { label }
fn from_alpha() string { return label() }
') or { panic(err) }
	os.write_file(os.join_path(root, 'b.v'), r'module main
import beta { label }
fn from_beta() string { return label() }
') or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, r'module main
import alpha { label, make_value, describe }
fn main() {
	assert from_alpha() == "alpha"
	assert from_beta() == "beta"
	value := make_value(17)
	assert describe(value) == "number=17"
	callback := label
	assert callback() == "alpha"
	assert "${label()}" == "alpha"
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		executable := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o', executable,
			root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
	os.write_file(main_path, r'module main
import alpha { hidden }
fn main() { println(hidden()) }
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o',
			os.join_path(root, 'private_${backend}'), main_path])
		assert build.exit_code != 0, backend
		assert build.output.contains('private'), '${backend}: ${build.output}'
	}
}
