// vtest build: !windows
import os
import v.cmdexec

fn test_smoke_streams_nested_output_before_the_command_exits() {
	root := os.join_path(os.vtmp_dir(), 'nested smoke progress ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fake_source := os.join_path(root, 'fake.v')
	fake_compiler := os.join_path(root, 'fake compiler')
	smoke := os.join_path(root, 'smoke')
	marker := 'NESTED_SMOKE_CHILD_STARTED'
	os.write_file(fake_source, 'import os
import time
fn main() {
	if os.args.len > 1 && os.args[1] == "test" {
		println("${marker}")
		flush_stdout()
		time.sleep(time.minute)
	}
}
')!
	for source, binary in {
		fake_source:                                                              fake_compiler
		os.join_path(@VMODROOT, 'cmd', 'tools', 'test_if_v_test_system_works.v'): smoke
	} {
		built := cmdexec.run_with_timeout(@VEXE, ['-o', binary, source], 180_000)
		assert built.exit_code == 0, built.output
	}
	mut previous := map[string]string{}
	for name in ['VEXE', 'VTMP'] {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
	}
	defer {
		for name in ['VEXE', 'VTMP'] {
			if value := previous[name] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	os.setenv('VEXE', fake_compiler, true)
	os.setenv('VTMP', root, true)
	// Bound the harness from outside, as its watchdog would. The child remains
	// alive for a minute, so its output must be forwarded before it exits.
	result := cmdexec.run_with_timeout(smoke, [], 5_000)
	assert result.exit_code != 0, result.output
	assert result.output.contains('check_ok cmd:'), result.output
	assert result.output.contains(marker), result.output
	assert !result.output.contains('> all done'), result.output
}
