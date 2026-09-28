import os

const invalid_assignment_vexe = @VEXE
const invalid_assignment_tests_dir = os.dir(@FILE)
const invalid_assignment_v3_dir = os.dir(invalid_assignment_tests_dir)
const invalid_assignment_vlib_dir = os.dir(invalid_assignment_v3_dir)
const invalid_assignment_v3_src = os.join_path(invalid_assignment_v3_dir, 'v.v')

fn invalid_assignment_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_invalid_assignment_expression_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${invalid_assignment_vexe} -gc none -path "${invalid_assignment_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${invalid_assignment_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn invalid_assignment_run_bad(v3_bin string, name string, source string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('unexpected assignment operator `=`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_invalid_assignment_expression_is_rejected() {
	v3_bin := invalid_assignment_build_v3()
	cases := [
		'fn main() {
	a := 10
	b := 50
	x := (((((a & b = == 0)
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := (a & b =) == 100
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := a & b = == && println("hello")
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := a = b
	println(x)
}
',
	]
	for i, source in cases {
		invalid_assignment_run_bad(v3_bin, 'invalid_assignment_expression_${i}', source)
	}
}
