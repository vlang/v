import os

const imported_sum_map_vexe = @VEXE
const imported_sum_map_tests_dir = os.dir(@FILE)
const imported_sum_map_v3_dir = os.dir(imported_sum_map_tests_dir)
const imported_sum_map_vlib_dir = os.dir(imported_sum_map_v3_dir)
const imported_sum_map_v3_src = os.join_path(imported_sum_map_v3_dir, 'v3.v')

fn test_imported_sum_map_assignment_wraps_concrete_struct_value() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_imported_sum_map_test_${pid}')
	build :=
		os.execute('${imported_sum_map_vexe} -gc none -path "${imported_sum_map_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${imported_sum_map_v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_imported_sum_map_project_${pid}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'store')) or { panic(err) }
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

	out := os.join_path(os.temp_dir(), 'v3_imported_sum_map_out_${pid}')
	compile := os.execute('${v3_bin} -o ${out} ${main_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}
