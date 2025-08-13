import os

const vexe_path = @VEXE
const vexe = os.quoted_path(vexe_path)
const vroot = os.dir(vexe_path)

fn test_run_examples_good() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	cmd := '${vexe} doc -comments -run-examples cmd/tools/vdoc/testdata/run_examples_good/main.v'
	println('${@METHOD:30} running ${cmd} ...')
	res := os.execute(cmd)
	assert res.exit_code == 0
	assert res.output.contains('module main'), res.output
	assert res.output.contains('fn abc()'), res.output
	assert res.output.contains("abc just prints 'xyz'"), res.output
	assert res.output.contains('and should succeed'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 25'), res.output
}

fn test_run_examples_bad() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	cmd := '${vexe} doc -comments -run-examples cmd/tools/vdoc/testdata/run_examples_bad/main.v'
	println('${@METHOD:30} running ${cmd} ...')
	res := os.execute(cmd)
	assert res.exit_code != 0
	assert res.output.contains('error in documentation example'), res.output
	assert res.output.contains(' left value: 5 * 5 = 25'), res.output
	assert res.output.contains('right value: 77'), res.output
	assert res.output.contains('V panic: Assertion failed...'), res.output
	assert res.output.contains('module main'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 77'), res.output
}
