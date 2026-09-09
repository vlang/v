import os
import time

const vexe = @VEXE
const vroot = os.dir(vexe)
const tdir = os.join_path(os.vtmp_dir(), 'vwatch_test_24571')
const tsource_dir = os.join_path(tdir, 'src')
const toutput_dir = os.join_path(tdir, 'out')
// `v watch` may first rebuild vwatch; four concurrent musl builds can take more than 30 seconds.
const watch_ready_timeout = 2 * time.minute
const watch_poll_interval = 100 * time.millisecond

fn testsuite_begin() {
	os.rmdir_all(tdir) or {}
	os.mkdir_all(tsource_dir)!
	os.mkdir_all(toutput_dir)!
}

fn testsuite_end() {
	os.rmdir_all(tdir) or {}
}

fn test_watch_keeps_backend_flag_intact() {
	source_path := os.join_path(tsource_dir, 'hello.v')
	output_path := os.join_path(toutput_dir, 'hello.js')
	os.write_file(source_path, "fn main() {\n\tprintln('Hello world')\n}\n")!

	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_redirect_stdio()
	process.use_pgroup = true
	process.set_args(['watch', '-backend', 'js_browser', '-output', output_path, source_path])
	process.run()

	wait_error := wait_for_output(output_path, mut process)
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()

	assert wait_error == '', '${wait_error}\n${output}'
	assert output.contains(' -backend js_browser '), output
	assert !output.contains('-baend'), output
	assert !output.contains('Unknown argument `-baend`'), output
}

// Regression test for https://github.com/vlang/v/issues/27463 :
// `v watch run main.v` should recompile and rerun the program, when its source changes.
fn test_watch_run_reloads_on_source_change() {
	source_path := os.join_path(tsource_dir, 'reload.v')
	marker_path := os.join_path(toutput_dir, 'reload_marker.txt')
	os.rm(marker_path) or {}
	write_versioned_source(source_path, 'V1')!

	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_redirect_stdio()
	process.use_pgroup = true
	// the marker_path argument is passed through to the compiled and run program (see write_versioned_source):
	process.set_args(['watch', 'run', source_path, marker_path])
	process.run()

	// wait for the first compile+run to write the marker file:
	first_run_error := wait_for_marker(marker_path, 'V1', 'initial V1 run', mut process)
	mut reload_error := ''
	if first_run_error == '' {
		// os.file_last_mod_unix has a 1 second resolution, so make sure the edit lands in a later second,
		// otherwise the change detection loop can not notice that the source file was modified at all:
		time.sleep(1500 * time.millisecond)
		write_versioned_source(source_path, 'V2')!

		// the watcher should detect the change, recompile, and rerun, updating the marker to V2:
		reload_error = wait_for_marker(marker_path, 'V2', 'V2 reload', mut process)
	}

	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()

	assert first_run_error == '', '${first_run_error}\n${output}'
	assert reload_error == '', '${reload_error}\n${output}'
}

fn write_versioned_source(source_path string, version string) ! {
	os.write_file(source_path,
		"import os\nfn main() {\n\tos.write_file(os.args[1], '${version}') or { panic(err) }\n}\n")!
}

fn wait_for_output(output_path string, mut process os.Process) string {
	stopwatch := time.new_stopwatch()
	for {
		if os.exists(output_path) {
			return ''
		}
		if !process.is_alive() {
			return 'v watch exited after ${stopwatch.elapsed()} before creating `${output_path}`'
		}
		if stopwatch.elapsed() >= watch_ready_timeout {
			return 'timed out after ${watch_ready_timeout} waiting for v watch to create `${output_path}`; process is still alive'
		}
		time.sleep(watch_poll_interval)
	}
	return 'unreachable'
}

fn wait_for_marker(marker_path string, expected string, phase string, mut process os.Process) string {
	stopwatch := time.new_stopwatch()
	for {
		content := os.read_file(marker_path) or { '' }
		if content == expected {
			return ''
		}
		if !process.is_alive() {
			return 'v watch exited after ${stopwatch.elapsed()} during ${phase}; expected `${expected}` in `${marker_path}`, got `${content}`'
		}
		if stopwatch.elapsed() >= watch_ready_timeout {
			return 'timed out after ${watch_ready_timeout} during ${phase}; expected `${expected}` in `${marker_path}`, got `${content}`; process is still alive'
		}
		time.sleep(watch_poll_interval)
	}
	return 'unreachable'
}
