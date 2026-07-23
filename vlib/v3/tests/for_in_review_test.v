import os
import rand

const for_in_review_vexe = @VEXE
const for_in_review_tests_dir = os.dir(@FILE)
const for_in_review_v3_dir = os.dir(for_in_review_tests_dir)
const for_in_review_vlib_dir = os.dir(for_in_review_v3_dir)
const for_in_review_v3_src = os.join_path(for_in_review_v3_dir, 'v3.v')

fn for_in_review_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_for_in_review_test')
}

fn testsuite_begin() {
	v3_bin := for_in_review_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
}

fn build_v3_for_in_review() string {
	v3_bin := for_in_review_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${for_in_review_vexe} -gc none -path "${for_in_review_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${for_in_review_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn for_in_review_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn for_in_review_run_good(v3_bin string, name string, src string) string {
	out := for_in_review_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${out}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(out)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn for_in_review_run_bad(v3_bin string, name string, src string, expected string) {
	out := for_in_review_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	result := os.execute('${v3_bin} ${src_path} -b c -o ${out}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn test_pointer_conditions_reject_plain_if_and_for() {
	v3_bin := build_v3_for_in_review()
	for_in_review_run_bad(v3_bin, 'pointer_if_condition',
		'fn main() {\n\tx := 1\n\tp := &x\n\tif p {\n\t\tprintln("bad")\n\t}\n}\n',
		'if condition must be `bool`, not `&int`')
	for_in_review_run_bad(v3_bin, 'pointer_for_condition',
		'fn main() {\n\tx := 1\n\tp := &x\n\tfor p {\n\t\tbreak\n\t}\n}\n',
		'if condition must be `bool`, not `&int`')
}

fn test_c_style_for_accepts_pointer_condition() {
	v3_bin := build_v3_for_in_review()
	out := for_in_review_run_good(v3_bin, 'c_style_pointer_condition',
		'fn main() {\n\tmut x := 1\n\tmut p := &x\n\tmut count := 0\n\tfor ; p; p = unsafe { nil } {\n\t\tcount++\n\t}\n\tprintln(count)\n}\n')
	assert out == '1'
}

fn test_optional_array_for_in_skips_none_payload() {
	v3_bin := build_v3_for_in_review()
	out := for_in_review_run_good(v3_bin, 'optional_array_for_in_guard',
		'fn maybe_values(ok bool) ?[]int {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [1, 2, 3]\n}\n\nfn main() {\n\tmut total := 0\n\tfor value in maybe_values(false) {\n\t\ttotal += value\n\t}\n\tprintln(int_str(total))\n\tfor value in maybe_values(true) {\n\t\ttotal += value\n\t}\n\tprintln(int_str(total))\n}\n')
	assert out == '0\n6'
}

fn test_optional_map_for_in_is_rejected_before_codegen() {
	v3_bin := build_v3_for_in_review()
	for_in_review_run_bad(v3_bin, 'optional_map_for_in',
		'fn maybe_values() ?map[string]int {\n\treturn none\n}\n\nfn main() {\n\tfor key, value in maybe_values() {\n\t\tprintln(key + int_str(value))\n\t}\n}\n',
		'cannot iterate over `?map[string]int`')
}
