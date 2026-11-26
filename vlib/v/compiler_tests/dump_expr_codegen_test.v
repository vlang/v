import os
import v.tempname

fn dump_expr_compact_c(s string) string {
	return s.replace('\t', '').replace(' ', '').replace('\n', '')
}

fn dump_expr_run(bin string, args []string) (string, string) {
	mut process := os.new_process(bin)
	process.set_args(args)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp().trim_space()
	stderr := process.stderr_slurp().trim_space()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	return stdout, stderr
}

fn test_dump_expr_codegen_and_output() {
	dir := os.join_path(os.vtmp_dir(), 'v3_dump_expr_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_dump_expr_checks')
	src := bin + '.v'
	source := "module main

import os

fn f() int {
	return dump(3)
}

fn take_int(n int) int {
	return n + 1
}

fn bump(n int) int { println('side'); return n + 1 }

fn check_transparent() {
	x := dump(1 + 2)
	assert x == 3
	mut n := 4
	n = dump(n + 2)
	assert n == 6
	assert f() == 3
	y := take_int(dump(6))
	assert y == 7
	z := dump(bump(0))
	assert z == 1
	println(int_str(x + n + f() + y + z))
}

fn check_stderr() {
	value := dump(3)
	println(int_str(value + 4))
}

fn check_pointer() {
	i := 42
	dump(i)
	ir := &i
	dump(ir)
	irr := &ir
	dump(irr)
	irrr := &irr
	dump(irrr)
}

fn main() {
	match os.args[1] {
		'transparent' { check_transparent() }
		'stderr' { check_stderr() }
		'pointer' { check_pointer() }
		else { panic('unknown dump fixture') }
	}
}
"
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, '-no-parallel', '-nocache', '-b', 'c', '-o', bin, src])
	assert compile.exit_code == 0, compile.output

	stdout, stderr := dump_expr_run(bin, ['transparent'])
	assert stdout.split_into_lines() == ['side', '20'], stdout
	assert stderr.len > 0

	label_stdout, label_stderr := dump_expr_run(bin, ['stderr'])
	assert label_stdout == '7', label_stdout
	assert label_stderr.ends_with('] 3: 3'), label_stderr

	pointer_stdout, pointer_stderr := dump_expr_run(bin, ['pointer'])
	assert pointer_stdout == '', pointer_stdout
	lines := pointer_stderr.split_into_lines()
	assert lines.len == 4, pointer_stderr
	assert lines[0].ends_with('] i: 42'), pointer_stderr
	assert lines[1].ends_with('] ir: &42'), pointer_stderr
	assert lines[2].ends_with('] irr: &&42'), pointer_stderr
	assert lines[3].ends_with('] irrr: &&&42'), pointer_stderr

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('dump('), generated
	assert !generated.contains('= ;'), generated
	transparent := dump_expr_compact_c(generated.all_after(' check_transparent(void) {').all_before('\n}'))
	returning := dump_expr_compact_c(generated.all_after(' f(void) {').all_before('\n}'))
	label := dump_expr_compact_c(generated.all_after(' check_stderr(void) {').all_before('\n}'))
	// V int is i64 on 64-bit targets, so literal arithmetic widens before evaluation.
	assert transparent.contains('i64__dump_0=((i64)(1)+(2));'), transparent
	assert transparent.contains('i64__dump_1=n+2;'), transparent
	assert transparent.contains('n=__dump_1;'), transparent
	assert transparent.contains('take_int(__dump_2)'), transparent
	assert transparent.contains('i64__dump_3=bump(0);'), transparent
	assert returning.contains('return__dump_0;'), returning
	// The label and value are joined with string__plus or a stack-buffered string_plus_many.
	assert label.contains('eprintln(string__plus(')
		|| label.contains('eprintln(({string__v3_internal_symbol_join_'), label
}

fn test_dump_expr_respects_nop_dump_and_preserves_evaluation() {
	dir := os.join_path(os.vtmp_dir(), 'v3_dump_expr_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_dump_expr_nop_dump')
	src := bin + '.v'
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
	compile := os.exec([@VEXE, '-no-parallel', '-nocache', '-d', 'nop_dump', '-b', 'c', '-o', bin,
		src])
	assert compile.exit_code == 0, compile.output

	stdout, stderr := dump_expr_run(bin, [])
	assert stdout == '42:1', stdout
	assert stderr == '', stderr

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('bump(mut calls): '), generated
	assert dump_expr_compact_c(generated).count('bump(&calls)') == 1, generated
}
