import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_option_arg_codegen_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')

	run := os.execute(bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

// test_optional_argument_codegen_wraps_values_and_none validates this v3 regression case.
fn test_optional_argument_codegen_wraps_values_and_none() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'option_arg_codegen_input',
		"struct Info {\n\tvalue int\n}\n\nfn pass_int(x ?int) int {\n\treturn 1\n}\n\nfn pass_info(x ?Info) int {\n\treturn 1\n}\n\nfn main() {\n\tassert pass_int(3) == 1\n\tassert pass_int(none) == 1\n\tassert pass_info(Info{value: 4}) == 1\n\tassert pass_info(none) == 1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

// test_optional_if_expr_codegen_initializes_optional_temp validates this v3 regression case.
fn test_optional_if_expr_codegen_initializes_optional_temp() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'optional_if_expr_codegen_input',
		"fn none_int() ?int {\n\treturn none\n}\n\nfn some_int() ?int {\n\treturn 7\n}\n\nfn choose(flag bool) ?int {\n\tvalue := if flag {\n\t\tnone_int()\n\t} else {\n\t\tsome_int()\n\t}\n\treturn value\n}\n\nfn main() {\n\tgood := choose(false) or { 0 }\n\tnone_value := choose(true) or { -1 }\n\tassert good == 7\n\tassert none_value == -1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

// test_optional_clone_return_wraps_payload validates this v3 regression case.
fn test_optional_clone_return_wraps_payload() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'optional_clone_return_codegen_input',
		"fn clone_array(src []int) ?[]int {\n\treturn src.clone()\n}\n\nfn clone_array_defer(src []int) ?[]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn clone_map(src map[string]int) ?map[string]int {\n\treturn src.clone()\n}\n\nfn clone_map_defer(src map[string]int) ?map[string]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn array_sum(values []int) int {\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\treturn total\n}\n\nfn main() {\n\tmut values := []int{}\n\tvalues << 2\n\tvalues << 5\n\tarr := clone_array(values) or { []int{} }\n\tarr_defer := clone_array_defer(values) or { []int{} }\n\tmut lookup := map[string]int{}\n\tlookup['a'] = 3\n\tlookup['b'] = 4\n\tcloned := clone_map(lookup) or { map[string]int{} }\n\tcloned_defer := clone_map_defer(lookup) or { map[string]int{} }\n\tassert array_sum(arr) == 7\n\tassert array_sum(arr_defer) == 7\n\tassert cloned['a'] + cloned['b'] == 7\n\tassert cloned_defer['a'] + cloned_defer['b'] == 7\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}
