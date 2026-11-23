// vtest build: !windows
import os
import time
import v.cmdexec

fn test_smoke_streams_nested_output_before_the_command_exits() {
	root := os.join_path(os.vtmp_dir(), 'nested smoke progress ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fake_compiler := os.join_path(root, 'fake compiler')
	smoke := os.join_path(root, 'smoke')
	marker := 'NESTED_SMOKE_CHILD_STARTED'
	os.write_file(fake_compiler, '#!/bin/sh
if [ "\$1" = test ]; then
	echo "${marker}"
	exec sleep 60
fi
')!
	os.chmod(fake_compiler, 0o755)!
	source := os.join_path(@VMODROOT, 'cmd', 'tools', 'test_if_v_test_system_works.v')
	built := cmdexec.run_with_timeout(@VEXE, ['-o', smoke, source], 180_000)
	assert built.exit_code == 0, built.output

	mut environment := os.environ()
	environment['VEXE'] = fake_compiler
	environment['VTMP'] = root
	mut process := os.new_process(smoke)
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.use_pgroup = true
	process.run()

	// The fake compiler stays alive after emitting the marker. Check for the
	// forwarded output before shutting down both processes.
	sw := time.new_stopwatch()
	mut output := ''
	mut streamed := false
	for sw.elapsed() < 5 * time.second && process.is_alive() {
		output += process.stdout_read()
		if output.contains(marker) {
			streamed = process.is_alive()
			break
		}
		time.sleep(10 * time.millisecond)
	}
	process.signal_pgkill()
	process.wait()
	output += process.stdout_read()
	process.close()
	assert streamed, 'expected the marker while the smoke tool was still running\n${output}'
	assert output.contains('check_ok cmd:'), output
	assert !output.contains('> all done'), output
}
