import os

const qvexe = os.quoted_path(@VEXE)

fn depend_on_command(cmd string) ? {
	path := os.find_abs_path_of_executable(cmd) or {
		println('skip: ${cmd} not found')
		return none
	}
	res := os.execute('${os.quoted_path(path)} --version')
	if res.exit_code != 0 {
		println('skip: ${cmd} does not support --version')
		return none
	}
	if !res.output.contains('GNU coreutils') {
		println('skip: ${cmd} is not from coreutils')
		return none
	}
}

fn test_normal_exit_without_timeout_echo() {
	depend_on_command('echo') or { return }
	ee := os.execute('${qvexe} timeout 0.2 echo')
	assert ee.exit_code == 0, ee.output
	res := os.execute('${qvexe} timeout 0.2 echo z123')
	assert res.exit_code == 0, res.output
	assert res.output.contains('z123')
}

fn test_normal_exit_without_timeout_sleep() {
	depend_on_command('sleep') or { return }
	res := os.execute('${qvexe} timeout 0.4 sleep 0.1')
	assert res.exit_code == 0, res.output
	assert res.output == ''
}

fn test_exit_with_timeout() {
	depend_on_command('sleep') or { return }
	res := os.execute('${qvexe} timeout 0.2 sleep 2')
	assert res.exit_code == 124, res.output
}
