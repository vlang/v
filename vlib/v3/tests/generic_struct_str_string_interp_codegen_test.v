import os

const generic_struct_str_vexe = @VEXE
const generic_struct_str_tests_dir = os.dir(@FILE)
const generic_struct_str_v3_dir = os.dir(generic_struct_str_tests_dir)
const generic_struct_str_vlib_dir = os.dir(generic_struct_str_v3_dir)
const generic_struct_str_v3_src = os.join_path(generic_struct_str_v3_dir, 'v3.v')

fn generic_struct_str_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_struct_str_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_struct_str_vexe} -gc none -path "${generic_struct_str_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_struct_str_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_struct_str_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_generic_struct_str_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'gr')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_struct_str' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'gr/gr.v'), 'module gr

pub struct Inner[T] {
mut:
	items []T
}

pub fn (mut inner Inner[T]) push(item T) {
	inner.items << item
}

pub fn (inner Inner[T]) str() string {
	return "inner=" + inner.items.str()
}

pub struct Outer[T] {
mut:
	inner Inner[T]
}

pub fn (mut outer Outer[T]) push(item T) {
	outer.inner.push(item)
}

pub fn (outer Outer[T]) str() string {
	return "outer=" + outer.inner.str()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import gr

fn main() {
	mut outer := gr.Outer[[]string]{}
	outer.push(['A', 'B'])
	interpolated := 'value \${outer}'
	direct := outer.str()
	assert interpolated == 'value ' + direct
	assert direct.len > 0
	println('ok')
}
") or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn test_generic_struct_str_is_used_for_string_interpolation_and_direct_calls() {
	v3_bin := generic_struct_str_build_v3()
	main_path := generic_struct_str_write_project()
	out := os.join_path(os.temp_dir(), 'v3_generic_struct_str_out_${os.getpid()}')
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('gr__Outer_Array_string__str'), generated
	assert generated.contains('gr__Inner_Array_string__str'), generated
	assert !generated.contains(' = outer;'), generated
	assert !generated.contains('Outer_T__str'), generated
	assert !generated.contains('Inner_T__str'), generated
}
