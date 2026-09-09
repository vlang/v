import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_assign_fixed_array_call_to_option_field() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_assign_test_${pid}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_assign_input_${pid}.v')
	os.write_file(src, 'struct Foo {
mut:
	data ?[2][3]u8
}

fn make() [2][3]u8 {
	return [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}

fn main() {
	mut foo := Foo{}
	foo.data = make()
	data := foo.data or { panic("missing fixed array") }
	assert data == [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}
')!

	bin := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_assign_input_${pid}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c')!
	assert c_code.contains('foo.data = ({ Optional_'), c_code
	assert c_code.contains('memcpy(') && c_code.contains('.value, (make()).ret_arr'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
}

fn test_indexed_optional_fixed_array_assignment_keeps_the_option_wrapper() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_index_assign_test_${pid}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_index_assign_input_${pid}.v')
	os.write_file(src, 'struct Empty {}

type Arr = [2]Empty

struct IndexCounter {
mut:
	calls int
}

fn next_index(mut counter IndexCounter) int {
	counter.calls++
	return 0
}

fn main() {
	mut values := []?Arr{len: 1}
	mut counter := IndexCounter{}
	values[next_index(mut counter)] = Arr{}
	assert counter.calls == 1
	assert values[0] != none
	assert values#[-1] != none
	values[next_index(mut counter)] = ?Arr(none)
	assert counter.calls == 2
	assert values[0] == none
	assert values#[-1] == none
	println("ok")
}
')!

	bin := os.join_path(os.temp_dir(), 'v3_optional_fixed_array_index_assign_input_${pid}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}

fn test_optional_map_index_comparison_preserves_evaluation_order() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_map_index_compare_test_${pid}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_map_index_compare_input_${pid}.v')
	os.write_file(src, 'struct Trace {
mut:
	value int
}

fn make_map(mut trace Trace) map[string]?int {
	trace.value = trace.value * 10 + 1
	return map[string]?int{}
}

fn make_key(mut trace Trace) string {
	trace.value = trace.value * 10 + 2
	return "key"
}

fn main() {
	mut trace := Trace{}
	assert make_map(mut trace)[make_key(mut trace)] == none
	assert trace.value == 12
	println("ok")
}
')!

	bin := os.join_path(os.temp_dir(), 'v3_optional_map_index_compare_input_${pid}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c')!
	map_pos := c_code.index('map __in_lhs_') or { panic('missing map temporary') }
	key_pos := c_code.index('string __map_key_') or { panic('missing map key temporary') }
	assert map_pos < key_pos, c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
