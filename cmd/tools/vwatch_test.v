import os
import time
import net.http

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

fn test_watch_run_keeps_the_windows_manifest_after_reload() ! {
	$if !windows {
		return
	}
	source := os.join_path(tsource_dir, 'manifest.v')
	marker := os.join_path(toutput_dir, 'manifest_marker.txt')
	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_redirect_stdio()
	process.use_pgroup = true
	process.set_args(['watch', '-cc', 'tcc', 'run', source, marker])
	mut errors := []string{}
	for version in ['V1', 'V2'] {
		if version == 'V2' { time.sleep(1500 * time.millisecond) }
		os.write_file(source, "module main
import os
fn main() {
	manifest := os.read_file(os.executable() + '.manifest') or { panic(err) }
	assert manifest.contains('longPathAware')
	os.write_file(os.args[1], '${version}') or { panic(err) }
}
")!
		if version == 'V1' { process.run() }
		failure := wait_for_marker(marker, version, 'manifest ${version}', mut process)
		if failure != '' {
			errors << failure
			break
		}
	}
	if process.is_alive() { process.signal_pgkill() }
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()
	assert errors.len == 0, errors.join('\n') + '\n' + output
}

// For veb apps, `v watch` builds the new version while the old one keeps serving,
// keeps the old one when a build fails, writes the build status for veb's live
// reload, and stops the app when it is itself terminated.
fn test_watch_builds_veb_app_first_and_stops_it_on_sigterm() {
	$if windows {
		return
	}
	app_dir := os.join_path(tsource_dir, 'vebapp')
	os.mkdir_all(app_dir)!
	port := 28000 + os.getpid() % 1000
	write_veb_app(app_dir, port, 'V1', true)!
	base := 'http://127.0.0.1:${port}'

	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_redirect_stdio()
	process.use_pgroup = true
	process.set_args(['watch', '-d', 'veb_livereload', 'run', app_dir])
	process.run()
	mut log := []string{}

	mut errors := []string{}
	if !wait_for_http(base + '/', 'version V1', mut process, mut log) {
		errors << 'the first build never served `version V1`'
	} else {
		// A broken build must leave the old version serving, and say so:
		time.sleep(1500 * time.millisecond)
		write_veb_app(app_dir, port, 'V2', false)!
		if !wait_for_log('build failed; the previous version keeps running', mut process, mut
			log)
		{
			errors << 'no `build failed` message after a broken edit'
		}
		if get_text(base + '/') != 'version V1' {
			errors << 'the old version stopped serving after a failed build'
		}
		if !get_text(base + '/status').starts_with('failed\n') {
			errors << 'the build status was not `failed`: `${get_text(base + '/status')}`'
		}
		// Fixing it swaps in the new version:
		time.sleep(1500 * time.millisecond)
		write_veb_app(app_dir, port, 'V3', true)!
		if !wait_for_http(base + '/', 'version V3', mut process, mut log) {
			errors << 'the fixed build never served `version V3`'
		}
		if get_text(base + '/status') != 'ok\n' {
			errors << 'the build status was not `ok`: `${get_text(base + '/status')}`'
		}
	}
	// SIGTERM to `v watch` itself (not its process group) must stop the app too:
	process.signal_term()
	stopwatch := time.new_stopwatch()
	for get_text(base + '/') != '' && stopwatch.elapsed() < 10 * time.second {
		time.sleep(watch_poll_interval)
	}
	if get_text(base + '/') != '' {
		errors << 'the app kept serving after `v watch` got SIGTERM'
	}
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	drain_process_output(mut process, mut log)
	process.close()
	assert errors.len == 0, errors.join('\n') + '\n' + log.join('')
}

fn write_veb_app(app_dir string, port int, version string, valid bool) ! {
	broken := if valid { '' } else { '\n\tthis does not compile' }
	os.write_file(os.join_path(app_dir, 'main.v'), "module main

import os
import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (app &App) index(mut ctx Context) veb.Result {${broken}
	return ctx.text('version ${version}')
}

pub fn (app &App) status(mut ctx Context) veb.Result {
	return ctx.text(os.read_file(os.getenv('VEB_LIVERELOAD_STATUS')) or { 'no status' })
}

fn main() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, family: .ip, host: '127.0.0.1', port: ${port}) or { panic(err) }
}
")!
}

fn get_text(url string) string {
	res := http.get(url) or { return '' }
	return res.body
}

fn drain_process_output(mut process os.Process, mut log []string) {
	for {
		chunk := process.stderr_read() + process.stdout_read()
		if chunk == '' {
			break
		}
		log << chunk
	}
}

fn wait_for_http(url string, expected string, mut process os.Process, mut log []string) bool {
	stopwatch := time.new_stopwatch()
	for stopwatch.elapsed() < watch_ready_timeout {
		drain_process_output(mut process, mut log)
		if get_text(url) == expected {
			return true
		}
		if !process.is_alive() {
			return false
		}
		time.sleep(watch_poll_interval)
	}
	return false
}

fn wait_for_log(text string, mut process os.Process, mut log []string) bool {
	stopwatch := time.new_stopwatch()
	for stopwatch.elapsed() < watch_ready_timeout {
		drain_process_output(mut process, mut log)
		if log.join('').contains(text) {
			return true
		}
		if !process.is_alive() {
			return false
		}
		time.sleep(watch_poll_interval)
	}
	return false
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
