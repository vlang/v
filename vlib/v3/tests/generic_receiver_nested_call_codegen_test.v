import os

const generic_receiver_nested_vexe = @VEXE
const generic_receiver_nested_tests_dir = os.dir(@FILE)
const generic_receiver_nested_v3_dir = os.dir(generic_receiver_nested_tests_dir)
const generic_receiver_nested_vlib_dir = os.dir(generic_receiver_nested_v3_dir)
const generic_receiver_nested_v3_src = os.join_path(generic_receiver_nested_v3_dir, 'v3.v')

fn generic_receiver_nested_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_receiver_nested_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_receiver_nested_vexe} -gc none -path "${generic_receiver_nested_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_receiver_nested_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_receiver_nested_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_generic_receiver_nested_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'gr')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_receiver_nested' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'gr/gr.v'), 'module gr

pub struct Inner[T] {
mut:
	value T
	has   bool
}

pub fn (inner Inner[T]) is_empty() bool {
	return !inner.has
}

pub fn (mut inner Inner[T]) push(item T) {
	if inner.is_empty() {
		inner.value = item
		inner.has = true
		return
	}
	inner.value = item
}

pub fn (mut inner Inner[T]) pop() !T {
	if inner.is_empty() {
		return error("empty")
	}
	inner.has = false
	return inner.value
}

pub struct Outer[T] {
mut:
	inner Inner[T]
}

pub fn (mut outer Outer[T]) push(item T) {
	outer.inner.push(item)
}

pub fn (mut outer Outer[T]) pop() !T {
	return outer.inner.pop()
}

pub fn (outer Outer[T]) is_empty() bool {
	return outer.inner.is_empty()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import gr

fn main() {
	mut outer := gr.Outer[[]string]{}
	outer.push(['A'])
	assert !outer.is_empty()
	got := outer.pop() or { panic(err) }
	assert got[0] == 'A'
	assert outer.is_empty()
	println('ok')
}
") or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn test_generic_receiver_nested_calls_use_specialized_receiver_methods() {
	v3_bin := generic_receiver_nested_build_v3()
	main_path := generic_receiver_nested_write_project()
	out := os.join_path(os.temp_dir(), 'v3_generic_receiver_nested_out_${os.getpid()}')
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('gr__Inner_Array_string__push'), generated
	assert generated.contains('gr__Inner_Array_string__pop'), generated
	assert generated.contains('Optional_Array'), generated
	assert !generated.contains('Inner_T__'), generated
	assert !generated.contains('Outer_T__'), generated
	assert !generated.contains('Optional_int __return_opt'), generated
}
