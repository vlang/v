import os

const shared_array_vexe = @VEXE
const shared_array_tests_dir = os.dir(@FILE)
const shared_array_v3_dir = os.dir(shared_array_tests_dir)
const shared_array_vlib_dir = os.dir(shared_array_v3_dir)
const shared_array_v3_src = os.join_path(shared_array_v3_dir, 'v3.v')

fn shared_array_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_shared_array_codegen_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${shared_array_vexe} -gc none -path "${shared_array_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${shared_array_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn shared_array_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn shared_array_write_project_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn shared_array_run_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	for rel, source in files {
		shared_array_write_project_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_bin_${os.getpid()}')
	input_path := os.join_path(root, input)
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_selector_shared_array_index_from_pointer_receiver() {
	v3_bin := shared_array_build_v3()
	out := shared_array_run_good(v3_bin, 'shared_array_pointer_receiver_index', 'import sync

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
	v3_bin := shared_array_build_v3()
	out := shared_array_run_project(v3_bin, 'shared_array_imported_field_push', {
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

fn test_shared_array_pointer_field_default_stays_nil() {
	v3_bin := shared_array_build_v3()
	out := shared_array_run_good(v3_bin, 'shared_array_pointer_field_default_nil', 'import sync

struct Item {
	value int
}

struct Holder {
mut:
	label  string
	items  &[]shared Item
	values []int
}

fn root_rwmutex_init() {
	mut m := sync.new_rwmutex()
	m.lock()
	m.unlock()
}

fn main() {
	root_rwmutex_init()
	h := Holder{}
	if isnil(h.items) {
		print("nil,")
	} else {
		print("set,")
	}
	println(int_str(h.values.len))
}
')
	assert out == 'nil,0'
}
