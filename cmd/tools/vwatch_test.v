import os
import time

const vexe = @VEXE
const vroot = os.dir(vexe)
const tdir = os.join_path(os.vtmp_dir(), 'vwatch_test_24571')
const tsource_dir = os.join_path(tdir, 'src')
const toutput_dir = os.join_path(tdir, 'out')

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

	for _ in 0 .. 300 {
		if os.exists(output_path) || !process.is_alive() {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()

	assert os.exists(output_path), output
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
	first_run_done := wait_for_marker(marker_path, 'V1')

	// os.file_last_mod_unix has a 1 second resolution, so make sure the edit lands in a later second,
	// otherwise the change detection loop can not notice that the source file was modified at all:
	time.sleep(1500 * time.millisecond)
	write_versioned_source(source_path, 'V2')!

	// the watcher should detect the change, recompile, and rerun, updating the marker to V2:
	reloaded := wait_for_marker(marker_path, 'V2')

	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()

	assert first_run_done, 'the watched program was never compiled and run\n${output}'
	assert reloaded, 'the watcher did not recompile and rerun after the source file changed\n${output}'
}

fn write_versioned_source(source_path string, version string) ! {
	os.write_file(source_path,
		"import os\nfn main() {\n\tos.write_file(os.args[1], '${version}') or { panic(err) }\n}\n")!
}

fn wait_for_marker(marker_path string, expected string) bool {
	for _ in 0 .. 300 {
		content := os.read_file(marker_path) or { '' }
		if content == expected {
			return true
		}
		time.sleep(100 * time.millisecond)
	}
	return false
}
