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

fn write_project(name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(root) or {}
	for rel, src in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, src) or { panic(err) }
	}
	return root
}

fn gen_c(v3_bin string, name string, suffix string, src string) string {
	return gen_c_flags(v3_bin, name, suffix, src, '')
}

fn gen_c_flags(v3_bin string, name string, suffix string, src string, flags string) string {
	src_path := write_source(name, suffix, src)
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${flags} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C file ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn compile_and_run(v3_bin string, name string, suffix string, src string) os.Result {
	return compile_and_run_flags(v3_bin, name, suffix, src, '')
}

fn compile_and_run_flags(v3_bin string, name string, suffix string, src string, flags string) os.Result {
	src_path := write_source(name, suffix, src)
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${flags} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains("undefined reference to `main'"), compile.output
	return os.execute(bin_path)
}

fn compile_project_and_run(v3_bin string, name string, files map[string]string) (os.Result, string) {
	root := write_project(name, files)
	return compile_project_root_and_run(v3_bin, name, root)
}

fn compile_project_root_and_run(v3_bin string, name string, root string) (os.Result, string) {
	bin_path := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	run := os.execute(bin_path)
	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	return run, c_code
}

fn compile_expect_failure(v3_bin string, name string, suffix string, src string) os.Result {
	return compile_expect_failure_flags(v3_bin, name, suffix, src, '')
}

fn compile_expect_failure_flags(v3_bin string, name string, suffix string, src string, flags string) os.Result {
	src_path := write_source(name, suffix, src)
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	c_path := bin_path + '.c'
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${flags} ${src_path} -b c -o ${bin_path}')
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

	result_fail_cleanup := compile_and_run(v3_bin, 'harness_result_fail_cleanup', '_test.v', "fn testsuite_end() {
	println('end')
}

fn after_each() {
	println('after')
}

fn test_fail() ! {
	return error('bad')
}
")
	assert result_fail_cleanup.exit_code != 0
	assert result_fail_cleanup.output.contains('test failed: test_fail')
	assert result_fail_cleanup.output.contains('after')
	assert result_fail_cleanup.output.contains('end')

	option_fail := compile_and_run(v3_bin, 'harness_option_fail', '_test.v', 'fn test_fail() ? {
	return none
}
')
	assert option_fail.exit_code != 0
	assert option_fail.output.contains('test failed: test_fail')

	option_fail_cleanup := compile_and_run(v3_bin, 'harness_option_fail_cleanup', '_test.v', "fn testsuite_end() {
	println('end')
}

fn after_each() {
	println('after')
}

fn test_fail() ? {
	return none
}
")
	assert option_fail_cleanup.exit_code != 0
	assert option_fail_cleanup.output.contains('test failed: test_fail')
	assert option_fail_cleanup.output.contains('after')
	assert option_fail_cleanup.output.contains('end')

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

	generic_test := compile_expect_failure(v3_bin, 'harness_generic_test', '_test.v', 'fn test_generic[T]() {
}
')
	assert generic_test.output.contains('invalid test signature'), generic_test.output

	invalid_return := compile_expect_failure(v3_bin, 'harness_invalid_return', '_test.v', 'fn test_bad() int {
	return 1
}
')
	assert invalid_return.output.contains('invalid test signature'), invalid_return.output

	same_module := compile_and_run(v3_bin, 'harness_same_module', '_test.v', "module sample

fn test_one() {
	println('sample test')
}
")
	assert same_module.exit_code == 0, same_module.output
	assert same_module.output.trim_space() == 'sample test', same_module.output

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

	generic_hook := compile_expect_failure(v3_bin, 'harness_generic_hook', '_test.v', 'fn before_each[T]() {
}

fn test_one() {
}
')
	assert generic_hook.output.contains('invalid test hook signature'), generic_hook.output

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

fn test_v3_test_file_harness_rejects_top_level_stmt() {
	v3_bin := build_v3()
	src := "println('top')

fn test_one() {
	println('test')
}
"
	invalid := compile_expect_failure(v3_bin, 'harness_top_level_and_test', '_test.v', src)
	assert invalid.output.contains('executable top-level statements are not supported in test files'), invalid.output
}

fn test_v3_test_file_harness_owns_entrypoint_over_user_main() {
	v3_bin := build_v3()
	src := "fn main() {
	println('user-main')
}

fn test_one() {
	println('test')
}
"
	c_code := gen_c(v3_bin, 'harness_user_main', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('test_one();'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'test'
}

fn test_v3_test_file_harness_keeps_user_main_callable() {
	v3_bin := build_v3()
	src := "fn main() {
	println('user-main')
}

fn test_call_main() {
	main()
}
"
	c_code := gen_c(v3_bin, 'harness_user_main_callable', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('main__user_main'), c_code
	assert c_code.contains('main__user_main();'), c_code
	assert c_code.contains('test_call_main();'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_callable_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'user-main'
}

fn test_v3_test_file_harness_forward_declares_user_main() {
	v3_bin := build_v3()
	src := "fn test_call_main() {
	main()
}

fn main() {
	println('late-main')
}
"
	c_code := gen_c(v3_bin, 'harness_user_main_forward', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	proto_idx := c_code.index('void main__user_main(void);') or { -1 }
	test_idx := c_code.index('void test_call_main(void) {') or { -1 }
	assert proto_idx >= 0, c_code
	assert test_idx > proto_idx, c_code
	assert c_code.contains('main__user_main();'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_forward_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'late-main'
}

fn test_v3_test_file_harness_renames_user_main_without_collision() {
	v3_bin := build_v3()
	src := "fn main__user_main() {
	println('collision')
}

fn main() {
	println('user-main')
}

fn test_call_both() {
	main__user_main()
	main()
}
"
	c_code := gen_c(v3_bin, 'harness_user_main_collision', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('void main__user_main('), c_code
	assert c_code.contains('void main__user_main_1('), c_code
	assert c_code.contains('main__user_main();'), c_code
	assert c_code.contains('main__user_main_1();'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_collision_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'collision\nuser-main'
}

fn test_v3_test_file_harness_renames_user_main_fn_value() {
	v3_bin := build_v3()
	src := "fn main() {
	println('user-main')
}

fn test_fn_value_main() {
	f := main
	f()
}
"
	c_code := gen_c(v3_bin, 'harness_user_main_fn_value', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('main__user_main'), c_code
	assert c_code.contains('test_fn_value_main();'), c_code
	assert !c_code.contains('= main;'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_fn_value_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'user-main'
}

fn test_v3_test_file_harness_renames_user_main_fn_arg() {
	v3_bin := build_v3()
	src := "fn takes(cb fn ()) {
	cb()
}

fn main() {
	println('user-main')
}

fn test_fn_arg_main() {
	takes(main)
}
"
	c_code := gen_c(v3_bin, 'harness_user_main_fn_arg', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('takes(main__user_main);'), c_code
	assert !c_code.contains('takes(main);'), c_code
	run := compile_and_run(v3_bin, 'harness_user_main_fn_arg_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'user-main'
}

fn test_v3_test_file_harness_does_not_rename_shadowed_main_fn_value_call() {
	v3_bin := build_v3()
	src := "fn call_shadowed(main fn ()) {
	main()
}

fn local_cb() {
	println('local-cb')
}

fn main() {
	println('user-main')
}

fn test_shadowed_main() {
	call_shadowed(local_cb)
}
"
	c_code := gen_c(v3_bin, 'harness_shadowed_main_fn_value_call', '_test.v', src)
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('main();'), c_code
	assert !c_code.contains('main__user_main();'), c_code
	run := compile_and_run(v3_bin, 'harness_shadowed_main_fn_value_call_run', '_test.v', src)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'local-cb'
}

fn test_v3_test_file_harness_finds_top_level_comptime_block_tests() {
	v3_bin := build_v3()
	src := "$if v3_nested_harness ? {
	fn before_each() {
		println('before')
	}

	fn test_nested() {
		println('nested')
	}
}
"
	c_code := gen_c_flags(v3_bin, 'harness_nested_block', '_test.v', src, '-d v3_nested_harness')
	assert c_code.contains('before_each();'), c_code
	assert c_code.contains('test_nested();'), c_code
	run := compile_and_run_flags(v3_bin, 'harness_nested_block_run', '_test.v', src,
		'-d v3_nested_harness')
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'before\nnested'

	invalid := compile_expect_failure_flags(v3_bin, 'harness_nested_invalid', '_test.v', '$if v3_nested_harness ? {
	fn test_bad(i int) {
	}
}
',
		'-d v3_nested_harness')
	assert invalid.output.contains('invalid test signature'), invalid.output

	invalid_generic := compile_expect_failure_flags(v3_bin, 'harness_nested_generic_invalid',
		'_test.v', '$if v3_nested_harness ? {
	fn before_each[T]() {
	}

	fn test_nested[T]() {
	}
}
',
		'-d v3_nested_harness')
	assert invalid_generic.output.contains('invalid test hook signature'), invalid_generic.output
	assert invalid_generic.output.contains('invalid test signature'), invalid_generic.output
}

fn test_v3_top_level_main_preserves_file_import_alias_context() {
	v3_bin := build_v3()
	run, c_code := compile_project_and_run(v3_bin, 'harness_file_alias_context', {
		'v.mod':      'Module { name: "alias_context" }
'
		'a/a.v':      'module a

pub fn value() int {
	return 11
}
'
		'b/b.v':      'module b

pub fn value() int {
	return 22
}
'
		'aa_alias.v': 'module main

import a as m

a_value := m.value()
println(int_str(a_value))
'
		'zz_alias.v': 'module main

import b as m

b_value := m.value()
println(int_str(b_value))
'
	})
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split_into_lines()
	assert lines.len == 2, run.output
	assert '11' in lines, run.output
	assert '22' in lines, run.output
	assert c_code.contains('a__value()'), c_code
	assert c_code.contains('b__value()'), c_code
}

fn test_v3_top_level_match_lowers_before_cgen_main() {
	v3_bin := build_v3()
	run := compile_and_run(v3_bin, 'harness_top_level_match', '.v', "x := match 2 {
	2 {
		7
	}
	else {
		3
	}
}
println(int_str(x))
match 2 {
	2 {
		println('two')
	}
	else {}
}
")
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7\ntwo'
}

fn test_v3_top_level_defer_runs_after_synthetic_main_body() {
	v3_bin := build_v3()
	run := compile_and_run(v3_bin, 'harness_top_level_defer', '.v', "fn deferred_msg() string {
	return 'deferred'
}

defer {
	println(deferred_msg())
}

println('body')
")
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'body\ndeferred'
}
