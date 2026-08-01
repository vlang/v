import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const hello_src = os.join_path(tests_dir, 'testdata', 'hello.v')

// test_c_output_path_only_writes_c_file validates this v3 regression case.
fn test_c_output_path_only_writes_c_file() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_output_only_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	c_out := os.join_path(os.temp_dir(), 'v3_output_only.c')
	bin_out := c_out.all_before_last('.c')
	os.rm(c_out) or {}
	os.rm(bin_out) or {}

	mut compile := os.Result{}
	// Repeated no-GC runs exercise the checker struct-field pointer microcache.
	// Its pointer-to-integer field assignment used to be lowered as a managed
	// closure temporary, making this cgen path crash intermittently.
	for _ in 0 .. 16 {
		compile = os.execute('${v3_bin} -gc none -o ${c_out} ${hello_src}')
		assert compile.exit_code == 0, compile.output
	}
	assert os.exists(c_out)
	assert !os.exists(bin_out)
	assert compile.output.contains('cgen')
	assert !compile.output.contains('  > ')
	assert !compile.output.contains('tcc.exe')
	assert !compile.output.contains('cc -std=gnu11')
}
