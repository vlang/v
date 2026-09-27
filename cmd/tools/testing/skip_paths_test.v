module testing

import os
import time

fn test_session_matches_skip_paths_through_symlinks() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v skip paths ${os.getpid()}_${time.now().unix_nano()}')
	real_dir := os.join_path(root, 'real')
	link_dir := os.join_path(root, 'link')
	os.mkdir_all(real_dir)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	os.symlink(real_dir, link_dir)!
	source := os.join_path(real_dir, 'excluded_test.v')
	linked_source := os.join_path(link_dir, 'excluded_test.v')
	os.write_file(source, 'deliberately invalid V source\n')!
	for index, file in [source, linked_source] {
		mut session := TestSession{
			files:        [file]
			skip_files:   [if index == 0 { linked_source } else { source }]
			will_compile: true
			// Skipped files must never reach the compiler, even through a symlink.
			vexe:         os.join_path(root, 'missing-compiler')
			vroot:        os.dir(@VEXE)
			vtmp_dir:     os.join_path(root, 'session_${index}')
			exec_mode:    .compile_and_run
		}
		session.test()
		assert !session.has_failures()
		assert session.benchmark.nfail == 0
		assert session.benchmark.nok == 0
		assert session.benchmark.nskip == 1
	}
}
