import os
import v.cmdexec

fn test_pruned_generic_pointer_results_and_options_compile_locally_and_across_imports() {
	root := os.join_path(os.vtmp_dir(), 'optional_pointer_forward_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	declarations := 'pub struct Box[T] {
pub:
	value T
}
pub fn make_box[T](value T) !&Box[T] {
	return &Box[T]{value: value}
}
pub fn maybe_box[T](value T) ?&Box[T] {
	return &Box[T]{value: value}
}
'
	for imported in [false, true] {
		path := os.join_path(root, if imported { 'imported' } else { 'local' })
		os.mkdir_all(path) or { panic(err) }
		prefix := if imported { 'boxes.' } else { '' }
		mut source := ''
		if imported {
			os.mkdir_all(os.join_path(path, 'boxes')) or { panic(err) }
			os.write_file(os.join_path(path, 'boxes', 'boxes.v'), 'module boxes\n${declarations}') or {
				panic(err)
			}
			source = 'import boxes\n'
		} else {
			source = declarations
		}
		source += 'interface Value {
	get() int
}
struct Number {
	n int
}
fn (number Number) get() int { return number.n }
type NumberPointer = &Number
fn identity(value NumberPointer) !NumberPointer { return value }
fn unused() int {
	boolean := ${prefix}make_box(false) or { panic(err) }
	optional := ${prefix}maybe_box(false) or { panic("missing") }
	contract := ${prefix}make_box(Value(Number{n: 2})) or { panic(err) }
	pointer := ${prefix}maybe_box(&Number{n: 3}) or { panic("missing") }
	return if boolean.value || optional.value { 1 } else { contract.value.get() + pointer.value.n }
}
fn main() {
	integer := ${prefix}make_box(42) or { panic(err) }
	text := ${prefix}maybe_box("live") or { panic("missing") }
	assert integer.value == 42
	assert text.value == "live"
	number := identity(&Number{n: 7}) or { panic(err) }
	assert number.n == 7
	println("optional-pointers-ok")
}
'
		main_file := os.join_path(path, 'main.v')
		os.write_file(main_file, source) or { panic(err) }
		for serial in [false, true] {
			mut args := ['-b', 'c', '-cc', @CCOMPILER, '-cstrict', '-nocache', '-no-retry-compilation']
			if serial {
				args << '-no-parallel'
			}
			args << ['run', main_file]
			result := cmdexec.run(@VEXE, args)
			assert result.exit_code == 0, result.output
			assert result.output.trim_space().ends_with('optional-pointers-ok'), result.output
		}
	}
}
