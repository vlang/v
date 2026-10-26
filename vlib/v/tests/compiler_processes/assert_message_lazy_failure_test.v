module main

import os
import time

fn test_failing_assert_evaluates_and_prints_message_once() ! {
	root := os.join_path(os.vtmp_dir(), 'assert_lazy_failure_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for test_mode in [false, true] {
		name := if test_mode { 'main_test.v' } else { 'main.v' }
		source := os.join_path(root, name)
		entry := if test_mode { 'test_failure' } else { 'main' }
		program := "fn message(mut calls []int) string { calls[0]++; println('message calls: '+calls[0].str()); return 'message payload' }\nfn ${entry}() { mut calls := [0]; assert false, 'failure: \${message(mut calls)}' }\n"
		os.write_file(source, program)!
		result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-cc', @CCOMPILER, 'run', source])
		assert result.exit_code != 0, result.output
		assert result.output.count('message calls: 1') == 1, result.output
		assert result.output.contains('failure: message payload'), result.output
	}
}
