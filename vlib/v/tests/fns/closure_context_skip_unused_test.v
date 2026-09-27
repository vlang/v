import os

const vexe = @VEXE

fn missing_boehm_lib(output string) bool {
	mentions_gc := output.contains('libgc') || output.contains('-lgc')
		|| output.contains("library 'gc'") || output.contains('bdw-gc')
	return mentions_gc && (output.contains('was not found')
		|| output.contains('cannot find') || output.contains('not found')
		|| output.contains('No such file'))
}

fn closure_skip_unused_source() string {
	return [
		'module main',
		'import builtin.closure',
		'',
		'fn make_cb(seed int) fn () int {',
		'\tvalues := []int{len: 8, init: seed + index}',
		'\treturn fn [values] () int {',
		'\t\treturn values[0] + values[7]',
		'\t}',
		'}',
		'',
		'fn no_capture_work() {}',
		'',
		'fn consume(i int) int {',
		'\th := fn [i] (x int) int {',
		'\t\treturn i + x',
		'\t}',
		'\treturn h(i + 1)',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut total := 0',
		'\tlifetime.frame(fn () {',
		'\t\tcb := make_cb(20)',
		'\t\tassert cb() == 47',
		'\t})!',
		'\tlifetime.suspend(fn () {',
		'\t\tcb := make_cb(30)',
		'\t\tassert cb() == 67',
		'\t})!',
		'\tlifetime.suspend(no_capture_work)!',
		'\tfor i in 0 .. 64 {',
		'\t\ttotal += consume(i)',
		'\t}',
		'\tlifetime.reclaim_all()!',
		'\tlifetime.dispose()!',
		'\tassert total == 4096',
		'\tprintln(total)',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn run_closure_skip_unused_case(tmp_dir string, mode string) {
	source_path := os.join_path(tmp_dir, 'closure_skip_unused_${mode}.v')
	binary_path := os.join_path(tmp_dir, 'closure_skip_unused_${mode}')
	os.write_file(source_path, closure_skip_unused_source()) or { panic(err) }
	compile_cmd := '${os.quoted_path(vexe)} -skip-unused -gc ${mode} -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}'
	compile_res := os.execute(compile_cmd)
	if mode != 'none' && compile_res.exit_code != 0 && missing_boehm_lib(compile_res.output) {
		eprintln('skipping ${mode} closure skip-unused test: missing libgc')
		return
	}
	assert compile_res.exit_code == 0, compile_res.output
	if mode == 'boehm_leak' {
		return
	}
	run_res := os.execute(os.quoted_path(binary_path))
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.contains('4096'), run_res.output
}

fn test_closure_context_helpers_are_kept_with_skip_unused() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_context_skip_unused_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	for mode in ['boehm', 'boehm_leak', 'none'] {
		run_closure_skip_unused_case(tmp_dir, mode)
	}
}
