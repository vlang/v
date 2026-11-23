import os

const unbuffer_stdout_probe_arg = '--vtest-unbuffer-stdout'

fn testsuite_begin() {
	if os.args.len != 2 || os.args[1] != unbuffer_stdout_probe_arg {
		return
	}
	unbuffer_stdout()
	print('#---')
	print('\b\b\b> 00')
	print('\b\b\b> 01')
	println('\b\b\b done')
	exit(0)
}

fn test_unbuffer_stdout() {
	result := os.exec([os.executable(), unbuffer_stdout_probe_arg])
	assert result.exit_code == 0, result.output
	assert result.output.replace('\r\n', '\n') == '#---\b\b\b> 00\b\b\b> 01\b\b\b done\n'
}
