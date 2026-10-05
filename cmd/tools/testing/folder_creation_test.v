module testing

import os
import rand
import sync.pool

fn test_worker_reports_folder_creation_failure_before_compilation() {
	root := os.join_path(os.vtmp_dir(), 'test folder failure ${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	blocked := os.join_path(root, 'blocked')
	os.write_file(blocked, 'a file cannot contain a test folder')!
	file := os.join_path(root, 'probe_test.v')
	os.write_file(file, 'fn test_probe() {}')!
	for stats in [false, true] {
		mut session := TestSession{
			files:        [file]
			will_compile: true
			vexe:         os.join_path(root, 'missing-compiler')
			vroot:        @VEXEROOT
			vtmp_dir:     os.join_path(blocked, 'session')
			show_stats:   stats
			exec_mode:    .compile_and_run
			nmessages:    chan LogMessage{cap: 100}
		}
		session.init()
		session.benchmark.set_total_expected_steps(1)
		mut workers := pool.new_pool_processor(callback: worker_trunner, maxjobs: 1)
		workers.set_shared_context(&session)
		workers.work_on_items([file])
		assert session.has_failures()
		assert session.benchmark.nfail == 1
		assert session.benchmark.nok == 0
		assert session.benchmark.nskip == 0
		assert session.nmessages.len == 1
		message := <-session.nmessages
		assert message.kind == .fail
		assert message.file == os.real_path(file)
		assert message.message.contains('could not create test folder ${session.vtmp_dir}')
		assert os.read_file(blocked)! == 'a file cannot contain a test folder'
	}
}
