module testing

import os
import time

fn test_session_skips_js_tests_without_an_explicit_skip_list() {
	root := os.join_path(os.vtmp_dir(), 'v js session skip ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'disabled_test.js.v')
	os.write_file(source, 'deliberately invalid V source\n')!
	for index, stats in [false, true] {
		mut session := TestSession{
			files:        [source]
			will_compile: true
			// Any attempted compilation fails; a skip must not invoke this executable.
			vexe:         os.join_path(root, 'missing-compiler')
			vroot:        os.dir(@VEXE)
			vtmp_dir:     os.join_path(root, 'session_${index}')
			vargs:        if stats { '-stats' } else { '' }
			show_stats:   stats
			exec_mode:    .compile_and_run
		}
		assert session.skip_files.len == 0
		session.test()
		assert !session.has_failures()
		assert session.benchmark.nfail == 0
		assert session.benchmark.nok == 0
		assert session.benchmark.nskip == 1
	}
}
