module main

import os

fn require(ok bool, message string) {
	if !ok {
		eprintln(message)
		exit(95)
	}
}

fn check_spawn() {
	require(os.setenv('FENV_CHILD', 'from-parent', true) == 0, 'child environment setup failed')
	mut child := os.new_process(os.args[0])
	child.set_args(['child'])
	child.set_redirect_stdio()
	child.run()
	child.wait()
	message := child.stdout_slurp()
	status := child.code
	child.close()
	require(status == 0 && message == 'child environment passed\n', 'child did not inherit current environment')
}

fn main() {
	if os.args.len == 2 && os.args[1] == 'child' {
		require(os.getenv('FENV_CHILD') == 'from-parent', 'child environment value lost')
		println('child environment passed')
		return
	}
	check_spawn()
	println('environment spawn passed')
}
