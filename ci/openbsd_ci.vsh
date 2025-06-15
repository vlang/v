import os
import common { Task, exec }

fn v_doctor() {
	dump(os.getenv('PATH'))
	exec('v doctor')
	if common.is_github_job {
		exec('uname -mrs')
		exec('sysctl hw.model')
		exec('sysctl hw.ncpu')
		exec('sysctl hw.physmem')
		exec('sysctl hw.usermem')
		exec('whoami')
		exec('pwd')
		exec('ls -la')
		exec('git log -n1')
		exec('cc --version')
	}
}

fn verify_v_test_works() {
	exec('echo \$VFLAGS')
	exec('v cmd/tools/test_if_v_test_system_works.v')
	exec('./cmd/tools/test_if_v_test_system_works')
}

fn build_fast_script() {
	exec('cd cmd/tools/fast && v fast.v')
}

fn check_math() {
	exec('v -silent test vlib/math')
	println('Test the math module, using only the pure V versions,')
	println('                          without the .c.v overrides.')
	exec('v -silent -exclude @vlib/math/*.c.v test vlib/math')
}

fn check_compress() {
	exec('v -silent test vlib/compress')
}

fn run_essential_tests() {
	if common.is_github_job {
		exec('VTEST_JUST_ESSENTIAL=1 v -silent test-self')
	} else {
		exec('VTEST_JUST_ESSENTIAL=1 v -progress test-self')
	}
}

const all_tasks = {
	'v_doctor':            Task{v_doctor, 'Run v doctor'}
	'verify_v_test_works': Task{verify_v_test_works, 'Verify that v test works'}
	'build_fast_script':   Task{build_fast_script, 'Check that building fast.v works'}
	'check_math':          Task{check_math, 'Check the `math` module works'}
	'check_compress':      Task{check_compress, 'Check the `compress` module works'}
	'run_essential_tests': Task{run_essential_tests, 'Run only the essential tests'}
}

common.run(all_tasks)
