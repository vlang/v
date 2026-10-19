module c

import os
import v.cmdexec

// Compile an ordinary program: test mode retains all array helpers and would
// hide a helper reached only after compile-time method dispatch is expanded.
fn run_array_delete_last_source(name string, source string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'array_delete_last_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	compiled := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-no-retry-compilation', '-o',
		os.join_path(root, 'program'), path])
	assert compiled.exit_code == 0, compiled.output
	return cmdexec.run(os.join_path(root, 'program'), []string{})
}

fn test_delete_last_helper_reached_through_comptime_method_dispatch() {
	result := run_array_delete_last_source('dispatch', 'struct Model {
mut:
	items []int
}
fn (mut model Model) remove_last() {
	model.items.delete_last()
}
fn dispatch[T](mut model T) {
	$for method in T.methods {
		$if method.typ is fn () {
			model.$method()
		}
	}
}
fn main() {
	mut model := Model{items: [1, 2, 3]}
	data := model.items.data
	cap := model.items.cap
	dispatch(mut model)
	assert model.items == [1, 2]
	assert model.items.data == data
	assert model.items.cap == cap
	view := unsafe { model.items[..] }
	dispatch(mut model)
	assert model.items == [1]
	assert view == [1, 2]
	assert model.items.data != data
	dispatch(mut model)
	assert model.items.len == 0
	model.items << 4
	assert model.items == [4]
}
')
	assert result.exit_code == 0, result.output
}

fn test_delete_last_helper_reached_from_generic_mutable_array() {
	result := run_array_delete_last_source('generic', 'struct Model[T] {
mut:
	items []T
}
fn remove_last[T](mut items []T) {
	items.delete_last()
}
fn (mut model Model[T]) remove_last() {
	remove_last(mut model.items)
}
fn dispatch[T](mut model T) {
	$for method in T.methods {
		$if method.typ is fn () {
			model.$method()
		}
	}
}
fn main() {
	mut numbers := Model[int]{items: [1, 2]}
	dispatch(mut numbers)
	assert numbers.items == [1]
	mut words := Model[string]{items: ["first", "last"]}
	dispatch(mut words)
	assert words.items == ["first"]
}
')
	assert result.exit_code == 0, result.output
}

fn test_delete_last_empty_array_still_panics_after_comptime_dispatch() {
	result := run_array_delete_last_source('empty', 'struct Model {
mut:
	items []int
}
fn (mut model Model) remove_last() {
	model.items.delete_last()
}
fn dispatch[T](mut model T) {
	$for method in T.methods {
		$if method.typ is fn () {
			model.$method()
		}
	}
}
fn main() {
	mut model := Model{}
	dispatch(mut model)
	println("after delete_last")
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('array.delete_last: array is empty'), result.output
	assert !result.output.contains('after delete_last'), result.output
}
