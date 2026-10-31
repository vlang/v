module types

import os

fn test_own_module_function_values_work_in_assignments_arguments_and_closures() {
	directory := os.join_path(os.vtmp_dir(), 'own_module_function_values_${os.getpid()}')
	os.mkdir_all(os.join_path(directory, 'local'))!
	defer { os.rmdir_all(directory) or {} }
	os.write_file(os.join_path(directory, 'local', 'local.v'), 'module local
pub const answer = 42
pub fn answer() int { return 43 }
pub fn double(value int) int { return value * 2 }
fn offset(value int) int { return value + 100 }
fn apply(callback fn (int) int, value int) int { return callback(value) }
struct Holder { @[required] double fn (int) int }
pub fn assigned() int {
    callback := local.double
    return callback(3)
}
pub fn argument() int { return apply(local.double, 4) }
pub fn immediate() int {
    return (fn () int { return apply(local.double, 5) })()
}
pub fn private_value() int {
    callback := local.offset
    return callback(6)
}
pub fn field_value() int {
    holder := Holder{double: offset}
    return holder.double(7)
}
pub fn constant_value() int { return local.answer }
pub fn function_call() int { return local.answer() }
')!
	main := os.join_path(directory, 'main.v')
	os.write_file(main, 'module main
import local as imported
fn consume(callback fn (int) int) int { return callback(8) }
fn main() {
    assert imported.assigned() == 6
    assert imported.argument() == 8
    assert imported.immediate() == 10
    assert imported.private_value() == 106
    assert imported.field_value() == 107
    assert imported.constant_value() == 42
    assert imported.function_call() == 43
    callback := imported.double
    assert callback(9) == 18
    assert consume(imported.double) == 16
}
')!
	for check in [false, true] {
		mut args := [@VEXE, '-b', 'c']
		if check { args << '-check' }
		args << ['-o', os.join_path(directory, 'program'), main]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		if !check {
			run := os.exec([os.join_path(directory, 'program')])
			assert run.exit_code == 0, run.output
		}
	}
}

fn test_unknown_namespace_function_values_keep_source_diagnostics() {
	directory := os.join_path(os.vtmp_dir(), 'unknown_module_function_values_${os.getpid()}')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(directory) or {} }
	path := os.join_path(directory, 'main.v')
	os.write_file(path, 'module main\nfn helper() {}\nfn main() { callback := absent.helper\ncallback()\n}\n')!
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(directory, 'program'), path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('main.v:3:'), result.output
	assert result.output.contains('absent'), result.output
}

fn test_own_module_global_precedes_function_value() {
	directory := os.join_path(os.vtmp_dir(), 'own_module_global_values_${os.getpid()}')
	os.mkdir_all(os.join_path(directory, 'local'))!
	defer { os.rmdir_all(directory) or {} }
	main := os.join_path(directory, 'main.v')
	os.write_file(main, 'module main\nimport local\nfn main() { assert local.result() == 17 }\n')!
	for declaration in ['__global callback int = 17', '@[c_extern]\n__global callback int'] {
		os.write_file(os.join_path(directory, 'local', 'local.v'), 'module local
${declaration}
pub fn callback(value int) int { return value * 2 }
pub fn result() int {
    value := local.callback
    return value
}
')!
		result := os.exec([@VEXE, '-b', 'c', '-enable-globals', '-check', main])
		assert result.exit_code == 0, result.output
	}
}

fn test_main_qualified_function_values_exclude_builtin_declarations() {
	directory := os.join_path(os.vtmp_dir(), 'main_module_function_values_${os.getpid()}')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(directory) or {} }
	path := os.join_path(directory, 'main.v')
	os.write_file(path, 'module main\nfn double(value int) int { return value * 2 }\nfn main() { callback := main.double\nassert callback(7) == 14\n}\n')!
	positive := os.exec([@VEXE, '-b', 'c', 'run', path])
	assert positive.exit_code == 0, positive.output
	os.write_file(path, 'module main\nfn main() { callback := main.println\ncallback("unexpected builtin")\n}\n')!
	for check in [false, true] {
		mut args := [@VEXE, '-b', 'c']
		if check { args << '-check' }
		args << ['-o', os.join_path(directory, 'program'), path]
		negative := os.exec(args)
		assert negative.exit_code != 0, negative.output
		assert negative.output.contains('main.v:2:'), negative.output
		assert negative.output.contains('println'), negative.output
	}
}

fn test_own_module_function_values_use_canonical_module_leaf() {
	directory := os.join_path(os.vtmp_dir(), 'canonical_module_function_values_${os.getpid()}')
	for parent in ['acme', 'other'] {
		os.mkdir_all(os.join_path(directory, parent, 'local'))!
		os.write_file(os.join_path(directory, parent, 'local', 'local.v'), 'module local
pub fn double(value int) int { return value * 2 }
fn apply(callback fn (int) int, value int) int { return callback(value) }
pub fn result() int {
    callback := local.double
    return callback(7) + apply(local.double, 8)
}
')!
	}
	defer { os.rmdir_all(directory) or {} }
	path := os.join_path(directory, 'main.v')
	os.write_file(path, 'module main
import acme.local
import other.local as another
fn main() {
    assert local.result() == 30
    assert another.result() == 30
}
')!
	checked := os.exec([@VEXE, '-b', 'c', '-check', path])
	assert checked.exit_code == 0, checked.output
	result := os.exec([@VEXE, '-b', 'c', 'run', path])
	assert result.exit_code == 0, result.output
}
