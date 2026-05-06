import os
import common { Task, exec }

fn v_doctor() {
	if common.is_github_job {
		println('::group::vdoctor')
	} else {
		println('### vdoctor')
	}
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
		println('::endgroup::')
	}
}

fn build_v_with_prealloc() {
	if common.is_github_job {
		println('::group::Build v with prealloc')
	} else {
		println('### Build v with prealloc')
	}
	exec('v -cg -cstrict -o vstrict1 cmd/v')
	exec('./vstrict1 -o vprealloc -prealloc cmd/v')
	exec('./vprealloc run examples/hello_world.v')
	exec('./vprealloc -o v3 cmd/v')
	exec('./v3 -o v4 cmd/v')
	exec('./v4 -d debug_malloc -d debug_realloc -o vdebug1 cmd/v')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn verify_v_test_works() {
	if common.is_github_job {
		println('::group::Verify v test')
	} else {
		println('### Verify v test')
	}
	exec('echo \$VFLAGS')
	exec('v cmd/tools/test_if_v_test_system_works.v')
	exec('./cmd/tools/test_if_v_test_system_works')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn build_fast_script() {
	if common.is_github_job {
		println('::group::Build fast script')
	} else {
		println('### Build fast script')
	}
	exec('cd cmd/tools/fast && v fast.v')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn check_math() {
	if common.is_github_job {
		println('::group::Test vlib/math')
	} else {
		println('### Test vlib/math')
	}
	exec('v -silent test vlib/math')
	println('Test the math module, using only the pure V versions,')
	println('                          without the .c.v overrides.')
	exec('v -silent -exclude @vlib/math/*.c.v test vlib/math')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn check_compress() {
	if common.is_github_job {
		println('::group::Test vlib/compress')
	} else {
		println('### Test vlib/compress')
	}
	exec('v -silent test vlib/compress')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn test_inline_assembly() {
	if common.is_github_job {
		println('::group::Test inline Assembly')
	} else {
		println('### Test inline Assembly')
	}
	exec('v test vlib/v/slow_tests/assembly')
	if common.is_github_job {
		println('::endgroup::')
	}
}

fn run_essential_tests() {
	if common.is_github_job {
		println('::group::Run essential tests')
		exec('VTEST_JUST_ESSENTIAL=1 v -silent test-self')
		println('::endgroup::')
	} else {
		println('### Run essential tests')
		exec('VTEST_JUST_ESSENTIAL=1 v -progress test-self')
	}
}

fn build_examples() {
	if common.is_github_job {
		println('::group::Build examples')
		exec('v -W build-examples')
		println('::endgroup::')
	} else {
		println('### Build examples')
		exec('v -progress build-examples')
	}
}

const all_tasks = {
	'v_doctor':              Task{v_doctor, 'Run v doctor'}
	'build_v_with_prealloc': Task{build_v_with_prealloc, 'Build V with prealloc'}
	'verify_v_test_works':   Task{verify_v_test_works, 'Verify that v test works'}
	'build_fast_script':     Task{build_fast_script, 'Check that building fast.v works'}
	'check_math':            Task{check_math, 'Check the `math` module works'}
	'check_compress':        Task{check_compress, 'Check the `compress` module works'}
	'test_inline_assembly':  Task{test_inline_assembly, 'Test inline Assembly'}
	'run_essential_tests':   Task{run_essential_tests, 'Run only the essential tests'}
	'build_examples':        Task{build_examples, 'Build examples'}
}

common.run(all_tasks)
