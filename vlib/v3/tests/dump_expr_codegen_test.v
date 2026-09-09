import os

const dump_expr_vexe = @VEXE
const dump_expr_tests_dir = os.dir(@FILE)
const dump_expr_v3_dir = os.dir(dump_expr_tests_dir)
const dump_expr_vlib_dir = os.dir(dump_expr_v3_dir)
const dump_expr_v3_src = os.join_path(dump_expr_v3_dir, 'v3.v')

fn dump_expr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_dump_expr_codegen_test_${os.getpid()}')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	os.rm(v3_bin) or {}
	build :=
		os.execute('${dump_expr_vexe} -gc none -prealloc -path "${dump_expr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${dump_expr_v3_src}')
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
	compile := os.execute('${v3_bin} -no-parallel -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	mut process := os.new_process(bin)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp().trim_space()
	stderr := process.stderr_slurp().trim_space()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	lines := stdout.replace('\r\n', '\n').split('\n')
	assert lines == ['side', '20'], stdout
	assert stderr.len > 0
	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := dump_expr_compact_c(generated)
	assert !generated.contains('dump('), generated
	assert !generated.contains('= ;'), generated
	assert compact.contains('int__dump_0=1+2;'), generated
	assert compact.contains('int__dump_1=n+2;'), generated
	assert compact.contains('n=__dump_1;'), generated
	assert compact.contains('return__dump_0;'), generated
	assert compact.contains('take_int(__dump_2)'), generated
	assert compact.contains('int__dump_3=bump(0);'), generated
}

fn test_dump_expr_writes_to_stderr() {
	v3_bin := dump_expr_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_dump_expr_stderr_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'fn main() {',
		'	value := dump(3)',
		'	println(int_str(value + 4))',
		'}',
	].join('\n')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_dump_expr_stderr_${os.getpid()}')
	compile := os.execute('${v3_bin} -no-parallel -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output

	mut process := os.new_process(bin)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp().trim_space()
	stderr := process.stderr_slurp().trim_space()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	assert stdout == '7', stdout
	assert stderr.len > 0

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := dump_expr_compact_c(generated)
	assert compact.contains('eprintln(string__plus('), generated
}

fn test_dump_expr_respects_nop_dump_and_preserves_evaluation() {
	v3_bin := dump_expr_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_dump_expr_nop_dump_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'fn bump(mut calls []int) int {',
		'	calls[0]++',
		'	return 42',
		'}',
		'',
		'fn main() {',
		'	mut calls := [0]',
		'	value := dump(bump(mut calls))',
		"	println('\${value}:\${calls[0]}')",
		'}',
	].join('\n')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_dump_expr_nop_dump_${os.getpid()}')
	compile := os.execute('${v3_bin} -no-parallel -nocache -d nop_dump -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output

	mut process := os.new_process(bin)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp().trim_space()
	stderr := process.stderr_slurp().trim_space()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	assert stdout == '42:1', stdout
	assert stderr == '', stderr

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('bump(mut calls): '), generated
	assert dump_expr_compact_c(generated).count('bump(&calls)') == 1, generated
}

fn test_dump_expr_preserves_pointer_values() {
	v3_bin := dump_expr_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_dump_expr_pointer_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'fn main() {',
		'	i := 42',
		'	dump(i)',
		'	ir := &i',
		'	dump(ir)',
		'	irr := &ir',
		'	dump(irr)',
		'	irrr := &irr',
		'	dump(irrr)',
		'}',
	].join('\n')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_dump_expr_pointer_${os.getpid()}')
	compile := os.execute('${v3_bin} -no-parallel -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output

	mut process := os.new_process(bin)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp().trim_space()
	stderr := process.stderr_slurp().trim_space()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	assert stdout == '', stdout
	lines := stderr.replace('\r\n', '\n').split('\n')
	assert lines.len == 4, stderr
	assert lines[0].ends_with('] i: 42'), stderr
	assert lines[1].ends_with('] ir: &42'), stderr
	assert lines[2].ends_with('] irr: &&42'), stderr
	assert lines[3].ends_with('] irrr: &&&42'), stderr
}
