import os
import v.tempname

fn map_for_mut_run_good(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, '-enable-globals', '-b', 'c', '-o', bin, src])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_mut_map_iteration_preserves_map_updates() {
	out := map_for_mut_run_good('map_for_mut_updates', 'fn check_snapshot_updates_original_values() {
	mut m := map[string]int{}
	m["a"] = 1
	m["b"] = 1
	mut other := map[string]int{}
	other["x"] = 9
	for _, mut v in m {
		v = 2
		other.delete("x")
	}
	a := m["a"] or { 0 }
	b := m["b"] or { 0 }
	assert a == 2
	assert b == 2
}

struct Item {
	value int
}

fn check_pointer_slot_replacement() {
	first := &Item{value: 1}
	replacement := &Item{value: 2}
	mut values := map[string]&Item{}
	values["item"] = first
	for _, mut item in values {
		assert item.value == 1
		item = replacement
		assert item.value == 2
	}
	stored := values["item"] or { &Item{} }
	assert stored == replacement
	assert stored.value == 2
}

fn check_delete_current_key() {
	mut m := map[string][2]int{}
	m["a"] = [1, 0]!
	m["b"] = [2, 0]!
	for key, mut v in m {
		v[0] = 9
		m.delete(key)
	}
	assert m.len == 0
}

fn check_equal_value_reinsertion() {
	mut m := map[string][2]int{}
	m["a"] = [1, 0]!
	for key, mut v in m {
		v[0] = 9
		m.delete(key)
		m[key] = [1, 0]!
	}
	assert m["a"][0] == 1
}

fn check_explicit_update() {
	mut m := map[string][2]int{}
	m["a"] = [1, 0]!
	m["b"] = [2, 0]!
	for key, mut v in m {
		if key == "a" {
			v[0] = 9
			m[key] = [3, 0]!
		}
		m.delete("missing")
	}
	assert m["a"][0] == 3
}

fn check_copyback_before_continue() {
	mut m := map[string][2]int{}
	m["a"] = [1, 0]!
	m["b"] = [2, 0]!
	for key, mut v in m {
		if key == "a" {
			v = [1, 11]!
		} else {
			v = [2, 12]!
		}
		continue
	}
	a := m["a"] or { [0, 0]! }
	b := m["b"] or { [0, 0]! }
	assert a[1] == 11
	assert b[1] == 12
}

fn main() {
	check_snapshot_updates_original_values()
	check_pointer_slot_replacement()
	check_delete_current_key()
	check_equal_value_reinsertion()
	check_explicit_update()
	check_copyback_before_continue()
	println("ok")
}
')
	assert out == 'ok', out
}

fn test_mut_map_iteration_copies_back_before_return() {
	out := map_for_mut_run_good('map_for_mut_return_copyback', '__global array_values map[string][2]int
__global scalar_values map[string]int

fn update_void() {
	for _, mut v in array_values {
		v[0] = 7
		return
	}
}

fn update_value() int {
	for _, mut v in array_values {
		v[0] = 9
		return 9
	}
	return 0
}

fn check_fixed_array_return_copyback() {
	array_values = map[string][2]int{}
	array_values["x"] = [1, 2]!
	update_void()
	av := array_values["x"] or { [0, 0]! }
	assert av[0] == 7

	array_values = map[string][2]int{}
	array_values["x"] = [1, 2]!
	ret := update_value()
	bv := array_values["x"] or { [0, 0]! }
	assert ret == 9
	assert bv[0] == 9
	println("ok")
}

fn return_array_from_map() [2]int {
	for _, mut v in array_values {
		v[0] = 7
		return array_values["x"]
	}
	return [0, 0]!
}

fn return_int_from_array_map() int {
	for _, mut v in array_values {
		v[0] = 9
		return array_values["x"][0]
	}
	return 0
}

fn check_fixed_array_return_reads_copyback() {
	array_values = map[string][2]int{}
	array_values["x"] = [1, 2]!
	a := return_array_from_map()
	assert a[0] == 7
	av := array_values["x"] or { [0, 0]! }
	assert av[0] == 7

	array_values = map[string][2]int{}
	array_values["x"] = [1, 2]!
	b := return_int_from_array_map()
	assert b == 9
	bv := array_values["x"] or { [0, 0]! }
	assert bv[0] == 9
	println("ok")
}

fn return_int_from_scalar_map() int {
	for _, mut v in scalar_values {
		v = 7
		return scalar_values["x"]
	}
	return 0
}

fn check_scalar_return_reads_update() {
	scalar_values = map[string]int{}
	scalar_values["x"] = 1
	println(int_str(return_int_from_scalar_map()))
}

fn main() {
	check_fixed_array_return_copyback()
	check_scalar_return_reads_update()
	check_fixed_array_return_reads_copyback()
}
')
	assert out.split_into_lines() == ['ok', '7', 'ok'], out
}
