import os

const map_for_mut_vexe = @VEXE
const map_for_mut_tests_dir = os.dir(@FILE)
const map_for_mut_v3_dir = os.dir(map_for_mut_tests_dir)
const map_for_mut_vlib_dir = os.dir(map_for_mut_v3_dir)
const map_for_mut_v3_src = os.join_path(map_for_mut_v3_dir, 'v3.v')

fn map_for_mut_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_map_for_mut_snapshot_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${map_for_mut_vexe} -gc none -path "${map_for_mut_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${map_for_mut_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn map_for_mut_run_good(v3_bin string, name string, source string) string {
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

fn test_mut_map_iteration_snapshot_updates_original_values() {
	v3_bin := map_for_mut_build_v3()
	out := map_for_mut_run_good(v3_bin, 'map_for_mut_snapshot_updates_original', 'fn main() {
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
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_map_fixed_array_value_copyback_runs_before_continue() {
	v3_bin := map_for_mut_build_v3()
	out := map_for_mut_run_good(v3_bin, 'map_for_mut_fixed_array_continue_copyback', 'fn main() {
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
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_map_fixed_array_value_copyback_runs_before_return() {
	v3_bin := map_for_mut_build_v3()
	out := map_for_mut_run_good(v3_bin, 'map_for_mut_fixed_array_return_copyback', '__global values map[string][2]int

fn update_void() {
	for _, mut v in values {
		v[0] = 7
		return
	}
}

fn update_value() int {
	for _, mut v in values {
		v[0] = 9
		return 9
	}
	return 0
}

fn main() {
	values = map[string][2]int{}
	values["x"] = [1, 2]!
	update_void()
	av := values["x"] or { [0, 0]! }
	assert av[0] == 7

	values = map[string][2]int{}
	values["x"] = [1, 2]!
	ret := update_value()
	bv := values["x"] or { [0, 0]! }
	assert ret == 9
	assert bv[0] == 9
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_map_fixed_array_return_reads_copied_back_value() {
	v3_bin := map_for_mut_build_v3()
	out := map_for_mut_run_good(v3_bin, 'map_for_mut_fixed_array_return_reads_copyback', '__global values map[string][2]int

fn return_array_from_map() [2]int {
	for _, mut v in values {
		v[0] = 7
		return values["x"]
	}
	return [0, 0]!
}

fn return_int_from_map() int {
	for _, mut v in values {
		v[0] = 9
		return values["x"][0]
	}
	return 0
}

fn main() {
	values = map[string][2]int{}
	values["x"] = [1, 2]!
	a := return_array_from_map()
	assert a[0] == 7
	av := values["x"] or { [0, 0]! }
	assert av[0] == 7

	values = map[string][2]int{}
	values["x"] = [1, 2]!
	b := return_int_from_map()
	assert b == 9
	bv := values["x"] or { [0, 0]! }
	assert bv[0] == 9
	println("ok")
}
')
	assert out == 'ok'
}
