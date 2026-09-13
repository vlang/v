import os

const nonaddr_receiver_vexe = @VEXE
const nonaddr_receiver_tests_dir = os.dir(@FILE)
const nonaddr_receiver_v3_dir = os.dir(nonaddr_receiver_tests_dir)
const nonaddr_receiver_vlib_dir = os.dir(nonaddr_receiver_v3_dir)
const nonaddr_receiver_v3_src = os.join_path(nonaddr_receiver_v3_dir, 'v.v')

fn nonaddr_receiver_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_non_addressable_receiver_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${nonaddr_receiver_vexe} -gc none -path "${nonaddr_receiver_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${nonaddr_receiver_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn nonaddr_receiver_write_source(name string, lines []string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, lines.join('\n') + '\n') or { panic(err) }
	return src
}

fn nonaddr_receiver_generate_c(v3_bin string, name string, src string) string {
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	generate := os.execute('${v3_bin} -o ${c_path} ${src}')
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

fn nonaddr_receiver_run(v3_bin string, name string, src string) string {
	bin := os.join_path(os.temp_dir(), 'v3_${name}_bin')
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A method that takes its receiver by reference needs the address of that receiver. A
// field of a call result has no address in C, so the receiver has to be materialized
// before its address is taken. Taking the address directly emits `&(make()).field`, which
// a C compiler rejects with "cannot take the address of an rvalue". The bundled tcc
// accepts that expression, so the emitted C is inspected here rather than relying on the
// default C compiler to reject it.
fn test_receiver_field_of_a_call_result_is_materialized() {
	v3_bin := nonaddr_receiver_build_v3()
	src := nonaddr_receiver_write_source('receiver_field_of_call', [
		'struct Inner {',
		'\tvalues []string',
		'}',
		'',
		'fn (i &Inner) count() int {',
		'\treturn i.values.len',
		'}',
		'',
		'struct Middle {',
		'\tinner Inner',
		'}',
		'',
		'struct Outer {',
		'\tmiddle Middle',
		'}',
		'',
		'fn make() Outer {',
		'\treturn Outer{',
		'\t\tmiddle: Middle{',
		'\t\t\tinner: Inner{',
		"\t\t\t\tvalues: ['a', 'b']",
		'\t\t\t}',
		'\t\t}',
		'\t}',
		'}',
		'',
		'fn main() {',
		'\tprintln(make().middle.inner.count())',
		'\tprintln(make().middle.inner.count() + 1)',
		'}',
	])
	generated := nonaddr_receiver_generate_c(v3_bin, 'receiver_field_of_call', src)
	assert generated.contains('Inner__count(')
	assert !generated.contains('&(make()).middle.inner')
	assert nonaddr_receiver_run(v3_bin, 'receiver_field_of_call', src) == '2\n3'
}
