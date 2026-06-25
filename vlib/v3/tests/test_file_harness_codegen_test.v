import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_with(name string, flags string) string {
	v3_bin := os.join_path(os.temp_dir(), name)
	build :=
		os.execute('${vexe} -gc none ${flags} -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn build_v3() string {
	return build_v3_with('v3_test_file_harness_test', '')
}

fn write_source(name string, suffix string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}${suffix}')
	os.write_file(src_path, src) or { panic(err) }
	return src_path
}

fn gen_c(v3_bin string, name string, suffix string, src string) string {
	src_path := write_source(name, suffix, src)
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C file ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn compile_and_run(v3_bin string, name string, suffix string, src string) os.Result {
	src_path := write_source(name, suffix, src)
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains("undefined reference to `main'"), compile.output
	return os.execute(bin_path)
}

fn compile_expect_failure(v3_bin string, name string, suffix string, src string) os.Result {
	src_path := write_source(name, suffix, src)
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	c_path := bin_path + '.c'
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !os.exists(c_path), '${name}: generated C despite harness input failure'
	return compile
}

fn test_v3_generates_minimal_test_file_harness() {
	v3_bin := build_v3()
	order_src := "fn test_one() {
	println('one')
}

fn test_two() {
	println('two')
}
"
	order_c := gen_c(v3_bin, 'harness_order', '_test.c.v', order_src)
	assert order_c.contains('int main('), order_c
	one_idx := order_c.index('test_one();') or { -1 }
	two_idx := order_c.index('test_two();') or { -1 }
	assert one_idx >= 0, order_c
	assert two_idx > one_idx, order_c
	order_run := compile_and_run(v3_bin, 'harness_order_run', '_test.c.v', order_src)
	assert order_run.exit_code == 0, order_run.output
	assert order_run.output.trim_space() == 'one\ntwo'

	parallel_v3_bin := build_v3_with('v3_test_file_harness_parallel_test', '-d parallel')
	parallel_c := gen_c(parallel_v3_bin, 'harness_parallel_order', '_test.c.v', order_src)
	assert parallel_c.contains('int main('), parallel_c
	assert parallel_c.contains('test_one();'), parallel_c
	assert parallel_c.contains('test_two();'), parallel_c
	parallel_run := compile_and_run(parallel_v3_bin, 'harness_parallel_order_run', '_test.c.v',
		order_src)
	assert parallel_run.exit_code == 0, parallel_run.output
	assert parallel_run.output.trim_space() == 'one\ntwo'

	hooks_src := "fn testsuite_begin() {
	println('begin')
}

fn testsuite_end() {
	println('end')
}

fn before_each() {
	println('before')
}

fn after_each() {
	println('after')
}

fn test_option() ? {
	println('option')
	return
}

fn test_result() ! {
	println('result')
	return
}
"
	hooks_run := compile_and_run(v3_bin, 'harness_hooks', '_test.v', hooks_src)
	assert hooks_run.exit_code == 0, hooks_run.output
	assert hooks_run.output.trim_space() == 'begin\nbefore\noption\nafter\nbefore\nresult\nafter\nend'

	result_fail := compile_and_run(v3_bin, 'harness_result_fail', '_test.v', "fn test_fail() ! {
	return error('bad')
}
")
	assert result_fail.exit_code != 0
	assert result_fail.output.contains('test failed: test_fail')

	option_fail := compile_and_run(v3_bin, 'harness_option_fail', '_test.v', 'fn test_fail() ? {
	return none
}
')
	assert option_fail.exit_code != 0
	assert option_fail.output.contains('test failed: test_fail')

	assert_fail := compile_and_run(v3_bin, 'harness_assert_fail', '_test.v', 'fn test_fail() {
	assert false
}
')
	assert assert_fail.exit_code != 0
	assert assert_fail.output.contains('assert failed')

	invalid_param := compile_expect_failure(v3_bin, 'harness_invalid_param', '_test.v', 'fn test_bad(i int) {
}
')
	assert invalid_param.output.contains('invalid test signature'), invalid_param.output

	invalid_return := compile_expect_failure(v3_bin, 'harness_invalid_return', '_test.v', 'fn test_bad() int {
	return 1
}
')
	assert invalid_return.output.contains('invalid test signature'), invalid_return.output

	non_main := compile_expect_failure(v3_bin, 'harness_non_main_module', '_test.c.v', 'module sample

fn test_one() {
}
')
	assert non_main.output.contains('no runnable tests'), non_main.output

	result_hook := compile_expect_failure(v3_bin, 'harness_result_hook', '_test.v', 'fn before_each() ! {
	return
}

fn test_one() {
}
')
	assert result_hook.output.contains('invalid test hook signature'), result_hook.output

	option_hook := compile_expect_failure(v3_bin, 'harness_option_hook', '_test.v', 'fn after_each() ? {
	return
}

fn test_one() {
}
')
	assert option_hook.output.contains('invalid test hook signature'), option_hook.output

	ordinary_c := gen_c(v3_bin, 'harness_plain_file', '.v', "fn test_lonely() {
	println('lonely')
}
")
	assert !ordinary_c.contains('int main('), ordinary_c
	assert !ordinary_c.contains('test_lonely();'), ordinary_c
}
