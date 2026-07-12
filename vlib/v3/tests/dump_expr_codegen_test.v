import os

const dump_expr_vexe = @VEXE
const dump_expr_tests_dir = os.dir(@FILE)
const dump_expr_v3_dir = os.dir(dump_expr_tests_dir)
const dump_expr_vlib_dir = os.dir(dump_expr_v3_dir)
const dump_expr_v3_src = os.join_path(dump_expr_v3_dir, 'v3.v')

fn dump_expr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_dump_expr_codegen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${dump_expr_vexe} -gc none -path "${dump_expr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${dump_expr_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn dump_expr_compact_c(s string) string {
	return s.replace('\t', '').replace(' ', '').replace('\n', '')
}

fn test_dump_expr_is_transparent_for_c_oracle_output() {
	v3_bin := dump_expr_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_dump_expr_codegen_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'fn f() int {',
		'	return dump(3)',
		'}',
		'',
		'fn take_int(n int) int {',
		'	return n + 1',
		'}',
		'',
		"fn bump(n int) int { println('side'); return n + 1 }",
		'',
		'fn main() {',
		'	x := dump(1 + 2)',
		'	assert x == 3',
		'	mut n := 4',
		'	n = dump(n + 2)',
		'	assert n == 6',
		'	assert f() == 3',
		'	y := take_int(dump(6))',
		'	assert y == 7',
		'	z := dump(bump(0))',
		'	assert z == 1',
		'	println(int_str(x + n + f() + y + z))',
		'}',
	].join('\n')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_dump_expr_codegen_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().replace('\r\n', '\n').split('\n')
	assert lines == ['side', '20'], run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := dump_expr_compact_c(generated)
	assert !generated.contains('dump('), generated
	assert !generated.contains('= ;'), generated
	assert compact.count('1+2') == 1, generated
	assert compact.contains('n=n+2;'), generated
	assert compact.contains('return3;'), generated
	assert compact.contains('take_int(6)'), generated
	assert compact.count('bump(0)') == 1, generated
}
