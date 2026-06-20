import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_option_arg_codegen_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

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

fn test_optional_argument_codegen_wraps_values_and_none() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'option_arg_codegen_input',
		"struct Info {\n\tvalue int\n}\n\nfn pass_int(x ?int) int {\n\treturn 1\n}\n\nfn pass_info(x ?Info) int {\n\treturn 1\n}\n\nfn main() {\n\tassert pass_int(3) == 1\n\tassert pass_int(none) == 1\n\tassert pass_info(Info{value: 4}) == 1\n\tassert pass_info(none) == 1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}
