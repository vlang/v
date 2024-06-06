import os

const tfile = os.join_path(os.vtmp_dir(), 'unknown_options_output.c')

fn test_unknown_option_flags_no_run() {
	os.chdir(os.dir(@VEXE))!
	os.rm(tfile) or {}

	res1 := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} examples/hello_world.v --an-unknown-option')
	assert res1.exit_code == 1, res1.output
	assert res1.output.starts_with('Unknown argument')
	assert res1.output.contains('--an-unknown-option')
	assert !os.exists(tfile)

	res2 := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} --an-unknown-option examples/hello_world.v')
	assert res2.exit_code == 1, res2.output
	assert res2.output.starts_with('Unknown argument')
	assert res2.output.contains('--an-unknown-option')
	assert !os.exists(tfile)
}

fn test_unknown_option_flags_with_run() {
	res_run_o := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} run examples/hello_world.v --an-unknown-option')
	assert res_run_o.exit_code == 0, res_run_o.output
	assert res_run_o.output == '' // because of -o, there should not be an actual run, since compilation stopped after generating the .c file
	assert os.exists(tfile)
	os.rm(tfile) or {}

	res_run_no_o_unknown_before_run := os.execute('${os.quoted_path(@VEXE)} --an-unknown-option run examples/hello_world.v ')
	assert res_run_no_o_unknown_before_run.exit_code == 1, res_run_no_o_unknown_before_run.output
	assert res_run_no_o_unknown_before_run.output.contains('v: unknown option `--an-unknown-option`')
	assert !os.exists(tfile)

	res_run_no_o := os.execute('${os.quoted_path(@VEXE)} run examples/hello_world.v --an-unknown-option')
	assert res_run_no_o.exit_code == 0, res_run_no_o.output
	assert res_run_no_o.output.trim_space() == 'Hello, World!'
	assert !os.exists(tfile)
}
