import os
import v.tempname

fn test_shared_array_pointer_receiver_index_and_imported_field_push() {
	dir := os.join_path(os.vtmp_dir(), 'v3_shared_array_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	root := os.join_path(dir, 'v3_shared_array_imported_field_push')
	other_dir := os.join_path(root, 'other')
	os.mkdir_all(other_dir) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'shared_array_imported_field_push' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(other_dir, 'other.v'), 'module other

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
') or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import other
import sync

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

fn check_pointer_receiver_index() {
	mut h := Holder{
		items: []shared Item{}
	}
	h.items << Item{
		value: 7
	}
	println(int_str(h.first()))
}

fn main() {
	root_rwmutex_init()
	check_pointer_receiver_index()
	mut h := other.new_holder()
	h.items << other.Foo{
		value: 9
	}
	item := h.items[0]
	println(int_str(item.value))
}
') or { panic(err) }
	bin := os.join_path(dir, 'v3_shared_array_imported_field_push_bin')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['7', '9'], run.output
}
