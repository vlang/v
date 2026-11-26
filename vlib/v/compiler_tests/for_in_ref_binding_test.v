import os
import v.tempname

// Regression coverage for #28084: `for x in &arr` must bind elements by
// reference, not by value. The checker's `for_in_iterable_yields_ref` used to
// be a dead stub that always returned false, so `annotate_for_in` never
// recorded the loop variable as pointer-typed for this case and mutations
// through it were silently lost.

fn for_in_ref_binding_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_for_in_ref_binding_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn for_in_ref_binding_run_good(name string, src string) string {
	root := for_in_ref_binding_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_${name}')
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.exec([out])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn for_in_ref_binding_run_bad(name string, src string, expected string) {
	root := for_in_ref_binding_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_${name}')
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	result := os.exec([@VEXE, src_path, '-b', 'c', '-o', '${out}'])
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
}

// A local fixed array cannot be referenced outside `unsafe` at all (it is
// stack-stored), independent of for-in - this must keep failing the same way
// it does for any other `&` of a local fixed array.
fn test_for_in_ref_of_local_fixed_array_requires_unsafe() {
	for_in_ref_binding_run_bad('local_fixed_array_ref',
		'struct Item {\nmut:\n\tval int\n}\n\nfn main() {\n\tmut arr := [3]Item{}\n\tfor item in &arr {\n\t\tprintln(item.val)\n\t}\n}\n',
		'cannot reference fixed array `arr` outside `unsafe` blocks')
}

// A `mut` array *parameter* stores its underlying array via a pointer at the
// ABI level even without any `&` in the for-in line. That must not be
// mistaken for an explicit `for x in &arr` - plain `for item in arr` inside
// such a function still binds `item` by value.
fn test_for_in_over_mut_param_without_amp_is_still_by_value() {
	for_in_ref_binding_run_bad('mut_param_no_amp_by_value',
		'struct Item {\nmut:\n\tval int\n}\n\nfn touch(mut arr []Item) {\n\tfor item in arr {\n\t\titem.val = item.val * 1000\n\t}\n}\n\nfn main() {\n\tmut arr := []Item{}\n\tarr << Item{val: 1}\n\ttouch(mut arr)\n}\n',
		'`item` is immutable, declare it with `mut` to make it mutable')
}

fn test_for_in_reference_and_mutation_bindings() {
	out := for_in_ref_binding_run_good('for_in_ref_bindings', 'struct Item {
mut:
	val int
}

struct Test {
mut:
	buf [10]Item
}

// A struct field lets the loop take a reference to a fixed array.
fn check_fixed_array_field() {
	mut t := Test{}
	for i in 0 .. 10 {
		t.buf[i] = Item{val: i}
	}
	for item in &t.buf {
		unsafe {
			item.val = item.val * 100
		}
	}
	mut total := 0
	for item in t.buf {
		total += item.val
	}
	println(total)
}

// The dynamic array path also binds elements by reference.
fn check_dynamic_array() {
	mut arr := []Item{}
	for i in 0 .. 5 {
		arr << Item{val: i}
	}
	for item in &arr {
		unsafe {
			item.val = item.val * 100
		}
	}
	mut total := 0
	for item in arr {
		total += item.val
	}
	println(total)
}

// A mut loop variable aliases the array element.
fn check_mut_binding() {
	mut arr := [3]Item{}
	for i in 0 .. 3 {
		arr[i] = Item{val: i + 1}
	}
	for mut item in arr {
		item.val = item.val * 10
	}
	mut total := 0
	for item in arr {
		total += item.val
	}
	println(total)
}

// The indexed form binds the element by reference and the index by value.
fn check_indexed_ref() {
	mut arr := []Item{}
	for i in 0 .. 4 {
		arr << Item{val: i}
	}
	for idx, item in &arr {
		unsafe {
			item.val = item.val + idx
		}
	}
	mut total := 0
	for item in arr {
		total += item.val
	}
	println(total)
}

struct Container[T] {
mut:
	items []T
}

// The transform can use the iterable shape when a cloned loop variable lacks a checker type.
fn (mut c Container[T]) bump_all() {
	for item in &c.items {
		unsafe {
			item.val = item.val * 10
		}
	}
}

fn check_generic_container() {
	mut c := Container[Item]{}
	c.items << Item{val: 1}
	c.items << Item{val: 2}
	c.items << Item{val: 3}
	c.bump_all()
	mut total := 0
	for it in c.items {
		total += it.val
	}
	println(total)
}

fn main() {
	check_fixed_array_field()
	check_dynamic_array()
	check_mut_binding()
	check_indexed_ref()
	check_generic_container()
}
')
	assert out.split_into_lines() == ['4500', '1000', '60', '12', '60'], out
}
