import os

const module_fn_collision_vexe = @VEXE
const module_fn_collision_tests_dir = os.dir(@FILE)
const module_fn_collision_v3_dir = os.dir(module_fn_collision_tests_dir)
const module_fn_collision_vlib_dir = os.dir(module_fn_collision_v3_dir)
const module_fn_collision_v3_src = os.join_path(module_fn_collision_v3_dir, 'v3.v')

fn module_fn_collision_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_module_fn_collision_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${module_fn_collision_vexe} -gc none -path "${module_fn_collision_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${module_fn_collision_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn module_fn_collision_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_module_fn_collision_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'collisionmod')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'localmod')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'module_fn_collision' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'collisionmod/collisionmod.v'), 'module collisionmod

struct Node[T] {
	value T
}

pub struct Holder[T] {
	node &Node[T]
}

fn new_node[T](value T) &Node[T] {
	return &Node[T]{
		value: value
	}
}

pub fn make[T](value T) Holder[T] {
	return Holder[T]{
		node: new_node(value)
	}
}

pub fn value[T](holder Holder[T]) T {
	return holder.node.value
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'localmod/localmod.v'), 'module localmod

fn helper() int {
	return 41
}

pub fn use_helper() int {
	return helper() + 1
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import collisionmod
import localmod

fn main() {
	holder := collisionmod.make(7)
	assert collisionmod.value(holder) == 7
	assert localmod.use_helper() == 42
	mut values := map[string]int{}
	values['answer'] = 42
	assert values['answer'] == 42
	println('ok')
}
") or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn test_imported_module_fn_short_name_does_not_pollute_builtin_return_type() {
	v3_bin := module_fn_collision_build_v3()
	main_path := module_fn_collision_write_project()
	out := os.join_path(os.temp_dir(), 'v3_module_fn_collision_out_${os.getpid()}')
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} -nocache ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('mapnode* z = new_node();'), generated
	assert generated.contains('collisionmod__new_node_T_v_int'), generated
	assert generated.contains('localmod__helper()'), generated
	assert !generated.contains('collisionmod__Node_T* z = new_node();'), generated
	assert !generated.contains('Array_fixed_collisionmod__Node_T* z = new_node();'), generated
}
