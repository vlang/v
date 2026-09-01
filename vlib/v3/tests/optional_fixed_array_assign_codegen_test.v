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
