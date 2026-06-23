import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const hello_src = os.join_path(tests_dir, 'hello.v')

fn test_prod_flag_before_input_uses_optimized_c_compile() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_prod_flag_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	out_bin := os.join_path(os.temp_dir(), 'v3_prod_hello')
	compile := os.execute('${v3_bin} -prod ${hello_src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cc -std=gnu11 -O2')
	assert !compile.output.contains('tcc.exe')

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello world'
}
