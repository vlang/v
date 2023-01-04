import os

const (
	test = @VROOT + '/vlib/v/tests/testdata/test_array_bound.v'
)

fn direct(line string) {
	if !line.contains('\tmain__direct(') {
		return
	}
	trimmed := line.trim_space()
	if trimmed.contains('array_get') {
		assert trimmed == 'this should have been a direct access in ${test} line ${line}'
	}
}

fn access(line string) {
	if !line.contains('\tmain__access(') {
		return
	}
	trimmed := line.trim_space()
	if !trimmed.contains('array_get') {
		assert trimmed == 'this should have been an array access in ${test} line ${line}'
	}
}

fn test_array_optimisation() {
	mut args := []string{cap: 4}
	args << '-prod'
	args << test
	args << '-o'
	args << '-'

	mut p := os.new_process(@VEXE)
	p.set_args(args)
	p.set_redirect_stdio()
	p.run()
	stdout := p.stdout_slurp()
	p.wait()
	p.close()

	assert stdout.contains('// THE END.')

	for line in stdout.split('\n') {
		direct(line)
		access(line)
	}
	println('ok, could not detect any wrong memory access optimisation')
}
