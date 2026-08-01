import os

const aoc_compat_vexe = @VEXE
const aoc_compat_tests_dir = os.dir(@FILE)
const aoc_compat_v3_dir = os.dir(aoc_compat_tests_dir)
const aoc_compat_vlib_dir = os.dir(aoc_compat_v3_dir)
const aoc_compat_v3_src = os.join_path(aoc_compat_v3_dir, 'v3.v')

fn aoc_compat_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_aoc_compat_codegen_test')
}

fn testsuite_begin() {
	os.rm(aoc_compat_v3_bin_path()) or {}
}

fn aoc_compat_build_v3() string {
	v3_bin := aoc_compat_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${aoc_compat_vexe} -gc none -path "${aoc_compat_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${aoc_compat_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn aoc_compat_run(name string, source string) string {
	v3_bin := aoc_compat_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_aoc_compat_${name}.v')
	bin := os.join_path(os.temp_dir(), 'v3_aoc_compat_${name}')
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_nested_map_mutations_write_back_to_outer_map() {
	out := aoc_compat_run('nested_map_mutations', "fn main() {
	mut values := map[string]map[string]int{}
	values['group'] = map[string]int{}
	values['group']['item']++
	values['group']['item'] += 2
	println(int_str(values['group']['item']))
	values['group'].delete('item')
	println(('item' in values['group']).str())
}
")
	assert out == '3\nfalse'
}

fn test_aoc_array_dsl_and_fixed_array_iteration() {
	out := aoc_compat_run('array_dsl_fixed_array', "import arrays
import math

const offsets = [
	[0, 1]!,
	[1, 0]!,
	[0, -1]!,
	[-1, 0]!,
]

fn main() {
	mut offset_total := 0
	for offset in offsets {
		offset_total += offset[0] + offset[1]
	}
	limit := 3
	values := [1, 2, 3, 4]
	println(values.any(fn [limit] (value int) bool {
		return value > limit
	}))
	println(values.all(fn [limit] (value int) bool {
		return value <= limit + 1
	}))
	println(int_str(values.count(fn [limit] (value int) bool {
		return value >= limit
	})))
	total := arrays.sum(['a', 'abcd'].map(fn (value string) int {
		return value.len
	}))!
	power := int(math.pow(10, 3 / 2 + 1))
	println(int_str(offset_total) + ',' + int_str(total) + ',' + int_str(power))
}
")
	assert out == 'true\ntrue\n2\n0,5,100'
}

fn test_generic_fn_value_return_preserves_interface_dispatch() {
	out := aoc_compat_run('generic_interface_fold', 'interface Counter {
	value int
	add(int) Counter
}

struct Number {
	value int
}

fn (number Number) add(delta int) Counter {
	return Counter(Number{
		value: number.value + delta
	})
}

fn fold[T, R](items []T, initial R, operation fn (R, T) R) R {
	mut value := initial
	for item in items {
		value = operation(value, item)
	}
	return value
}

fn main() {
	initial := Counter(Number{
		value: 1
	})
	result := fold([2, 3], initial, fn (state Counter, delta int) Counter {
		return state.add(delta)
	})
	println(int_str(result.value))
}
')
	assert out == '6'
}

fn test_attached_positional_struct_init_and_string_helpers() {
	out := aoc_compat_run('parser_string_helpers', "struct Point {
	x int
	y int
}

fn main() {
	if Point{2, 3}.x == 2 {
		rows := [['abc']]
		println(int_str(rows.first().first().len))
		println('banana'.contains('nan'))
		println(int_str('banana'.count('an')))
	}
}
")
	assert out == '3\ntrue\n2'
}
