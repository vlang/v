module testing

import os
import rand
import sync.pool

fn test_compiler_jobs_shares_available_cpus_between_tests() {
	assert test_compiler_jobs(3, 3, 3) == 1
	assert test_compiler_jobs(4, 2, 4) == 2
	assert test_compiler_jobs(8, 3, 8) == 2
	// Fewer selected tests and memory-limited sessions can use the spare CPUs.
	assert test_compiler_jobs(8, 1, 8) == 8
	assert test_compiler_jobs(18, 4, 18) == 4
	// Explicit VJOBS still limits both test workers and compiler workers.
	assert test_compiler_jobs(18, 3, 3) == 3
	assert test_compiler_jobs(18, 1, 1) == 1
	// The budget uses the cgroup quota rather than the host CPU count.
	assert test_compiler_jobs(effective_test_cpus(64, 3), 3, 64) == 1
	assert test_compiler_jobs(3, 7, 7) == 1
	assert test_compiler_jobs(0, 0, 0) == 1
}

fn test_compiler_jobs_preserves_test_program_environment() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '3', true)
	defer {
		if jobs := previous_jobs {
			os.setenv('VJOBS', jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	root := os.join_path(os.vtmp_dir(), 'compiler jobs ${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for stats in [false, true] {
		file := os.join_path(root, 'budget_${stats}_test.v')
		compiled_jobs := if stats { '3' } else { '1' }
		os.write_file(file, 'import os
import runtime
fn test_environment() {
	assert \$env("VJOBS") == "${compiled_jobs}"
	assert os.getenv("VJOBS") == "3"
	assert runtime.nr_jobs() == 3
	os.write_file(@FILE + ".executed", "passed")!
}
')!
		mut session := TestSession{
			files:         [file]
			will_compile:  true
			vexe:          @VEXE
			vroot:         @VEXEROOT
			vtmp_dir:      os.join_path(root, 'session_${stats}')
			vargs:         if stats { '-stats' } else { '' }
			show_stats:    stats
			exec_mode:     .compile_and_run
			nmessages:     chan LogMessage{cap: 100}
			compiler_jobs: 1
		}
		session.init()
		session.benchmark.set_total_expected_steps(1)
		mut workers := pool.new_pool_processor(callback: worker_trunner, maxjobs: 1)
		workers.set_shared_context(&session)
		workers.work_on_items([file])
		mut output := []string{}
		for session.nmessages.len > 0 {
			message := <-session.nmessages
			output << message.message
		}
		assert !session.has_failures(), output.join('\n')
		assert session.benchmark.nok == 1, output.join('\n')
		assert os.read_file(file + '.executed')! == 'passed'
		assert os.getenv('VJOBS') == '3'
	}
	// A formatting/tool session must also keep the original compiler environment.
	// With VTEST_SHOW_CMD=1 the command is logged first: the channel needs room,
	// as nothing reads it here.
	mut session := TestSession{
		compiler_jobs: 1
		nmessages:     chan LogMessage{cap: 100}
	}
	result := session.exec_compiler([@VEXE, os.join_path(root, 'budget_true_test.v')], {},
		MessageThreadContext{})
	assert result.exit_code == 0, result.output
	assert os.getenv('VJOBS') == '3'
}
