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

	for _ in 0 .. 80 {
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
