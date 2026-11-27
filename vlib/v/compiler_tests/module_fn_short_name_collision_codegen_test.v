import os
import v.tempname

fn module_fn_collision_write_project(root string) string {
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

fn new_map_data[T](value T) &Node[T] {
	return &Node[T]{
		value: value
	}
}

pub fn make[T](value T) Holder[T] {
	return Holder[T]{
		node: new_map_data(value)
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

pub fn run(command string, args []string) string {
	return command + args.join("")
}

struct Item {
	value string
}

fn load_item() !Item {
	return Item{
		value: "module"
	}
}

pub fn read_item() !string {
	item := load_item()!
	return item.value
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import collisionmod
import encoding.base64
import localmod

struct Runner {}

struct Item {
	value string
}

struct Author {
	first  string
	middle string
	last   string
}

fn load_item() !Item {
	return Item{
		value: 'main'
	}
}

fn encoded_authors(authors []Author) []string {
	mut values := []string{}
	for author in authors {
		values << [author.first, author.middle, author.last].map(base64.encode(it.bytes())).join(',')
	}
	return values
}

fn load_pair(fail bool) !(string, bool) {
	if fail {
		return error('no pair')
	}
	return 'pair', true
}

fn (mut r Runner) run() {
	_ = r
}

fn main() {
	holder := collisionmod.make(7)
	assert collisionmod.value(holder) == 7
	assert localmod.use_helper() == 42
	command := 'module-'
	args := ['call']
	assert localmod.run(command, args) == 'module-call'
	assert (localmod.read_item() or { panic(err) }) == 'module'
	assert (load_item() or { panic(err) }) == Item{
		value: 'main'
	}
	assert encoded_authors([Author{
		first: 'Ada'
		last: 'Lovelace'
	}]) == ['QWRh,,TG92ZWxhY2U=']
	value, _ := load_pair(false) or {
		eprintln(err.msg())
		exit(1)
	}
	assert value == 'pair'
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
	root := os.join_path(os.vtmp_dir(), 'v3_module_fn_collision_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := module_fn_collision_write_project(root)
	out := os.join_path(root, 'v3_module_fn_collision_out')
	compile := os.exec([@VEXE, '-nocache', main_path, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	generated := os.read_file(out + '.c') or { panic(err) }
	signature := 'VMapData* new_map_data(i64 key_bytes, i64 value_bytes, '
	storage := '.key_values = (DenseArray){' +
		'.key_bytes = key_bytes, .value_bytes = value_bytes'
	assert generated.contains(signature), generated
	assert generated.contains(storage), generated
	assert generated.contains('collisionmod__new_map_data_T_v_int'), generated
	assert generated.contains('localmod__helper()'), generated
	assert generated.contains('localmod__run(command, args)'), generated
	assert generated.contains('__v_result_localmod__Item localmod__load_item(void);'), generated
	assert !generated.contains('localmod__run(&command, args)'), generated
	assert !generated.contains('collisionmod__Node_int* new_map_data('), generated
	assert !generated.contains('Array_fixed_collisionmod__Node_int* new_map_data('), generated
}
