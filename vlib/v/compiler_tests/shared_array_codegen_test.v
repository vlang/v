import os
import v.tempname

fn shared_array_run_good(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_shared_array_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn shared_array_write_project_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn shared_array_run_project(name string, files map[string]string, input string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_shared_array_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	root := os.join_path(dir, 'v3_${name}')
	os.mkdir(root) or { panic(err) }
	for rel, source in files {
		shared_array_write_project_file(root, rel, source)
	}
	bin := os.join_path(dir, 'v3_${name}_bin')
	input_path := os.join_path(root, input)
	compile := os.exec([@VEXE, input_path, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_selector_shared_array_index_from_pointer_receiver() {
	out := shared_array_run_good('shared_array_pointer_receiver_index', 'import sync

struct Item {
	value int
}

struct Holder {
mut:
	items []shared Item
}

fn root_rwmutex_init() {
	mut m := sync.new_rwmutex()
	m.lock()
	m.unlock()
}

fn (h &Holder) first() int {
	item := h.items[0]
	return item.value
}

fn main() {
	root_rwmutex_init()
	mut h := Holder{
		items: []shared Item{}
	}
	h.items << Item{
		value: 7
	}
	println(int_str(h.first()))
}
')
	assert out == '7'
}

fn test_imported_shared_array_field_push_uses_owner_module() {
	out := shared_array_run_project('shared_array_imported_field_push', {
		'v.mod':         "Module { name: 'shared_array_imported_field_push' }\n"
		'other/other.v': 'module other

pub struct Foo {
pub:
	value int
}

pub struct Holder {
pub mut:
	items []shared Foo
}

pub fn new_holder() Holder {
	return Holder{
		items: []shared Foo{}
	}
}
'
		'main.v':        'module main

import other
import sync

fn root_rwmutex_init() {
	mut m := sync.new_rwmutex()
	m.lock()
	m.unlock()
}

fn main() {
	root_rwmutex_init()
	mut h := other.new_holder()
	h.items << other.Foo{
		value: 9
	}
	item := h.items[0]
	println(int_str(item.value))
}
'
	}, 'main.v')
	assert out == '9'
}
