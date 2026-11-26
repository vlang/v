import os
import v.tempname

fn test_imported_sum_map_assignment_wraps_concrete_struct_value() {
	root := os.join_path(os.vtmp_dir(), 'v3_imported_sum_map_project_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.mkdir(os.join_path(root, 'store'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'imported_sum_map' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'store/store.v'), 'module store

pub struct Item {
pub:
	n int
}

pub type Value = Item | int

pub struct Store {
pub mut:
	objects map[string]Value
}

pub fn (mut s Store) put(name string, n int) {
	s.objects[name] = Item{
		n: n
	}
}

pub fn (s Store) get(name string) int {
	value := s.objects[name] or { return 0 }
	if value is Item {
		return value.n
	}
	return 0
}
') or {
		panic(err)
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'module main

import store

fn main() {
	mut values := store.Store{}
	values.put("answer", 42)
	println(values.get("answer"))
}
') or {
		panic(err)
	}

	out := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '-o', out, main_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}
