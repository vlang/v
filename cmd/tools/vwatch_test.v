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

	mut wait_error := wait_for_output(output_path, mut process)
	// The watched compiler can create the output file before vwatch prints its command.
	// Wait for the log before stopping it, so the forwarded flag is observable.
	mut log := []string{}
	if wait_error == '' && !wait_for_log(' -backend js_browser ', mut process, mut log) {
		wait_error = 'v watch did not report the forwarded backend before exiting or timing out'
	}
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := log.join('') + process.stdout_slurp() + process.stderr_slurp()
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

// A veb project is watched through an implied filter of usual web file types, but a
// template can be of any type. The compiler reports it as an input of the program.
fn test_watch_rebuilds_veb_app_on_reported_inputs_of_any_type() {
	$if windows {
		return
	}
	app_dir := os.join_path(tsource_dir, 'vebtemplate')
	marker_path := os.join_path(toutput_dir, 'vebtemplate_marker.txt')
	page_path := os.join_path(app_dir, 'page.txt')
	partial_path := os.join_path(app_dir, 'partial.txt')
	os.mkdir_all(app_dir)!
	os.write_file(os.join_path(app_dir, 'main.v'), "module main

import os
import veb

pub struct Context {
	veb.Context
}

fn page() string {
	return \$tmpl('page.txt')
}

fn main() {
	os.write_file(os.args[1], page().trim_space()) or { panic(err) }
}
")!
	os.write_file(page_path, 'V1\n@include "partial.txt"\n')!
	os.write_file(partial_path, 'P1\n')!

	mut process := new_watch_process(['watch', 'run', app_dir, marker_path])
	mut failure := wait_for_marker(marker_path, 'V1\nP1', 'the first build', mut process)
	if failure == '' {
		failure = edit_and_wait(page_path, 'V2\n@include "partial.txt"\n', marker_path, 'V2\nP1', mut
			process)
	}
	if failure == '' {
		failure = edit_and_wait(partial_path, 'P2\n', marker_path, 'V2\nP2', mut process)
	}
	output := stop_watch_process(mut process)
	assert failure == '', '${failure}\n${output}'
}

// The built-in ignore list is meant for what compilers and editors leave behind. It
// has no say over a file that the compiler read to build the program.
fn test_watch_run_reloads_on_reported_input_without_extension() {
	project_dir := os.join_path(tsource_dir, 'extensionless')
	marker_path := os.join_path(toutput_dir, 'extensionless_marker.txt')
	page_path := os.join_path(project_dir, 'page')
	source_path := os.join_path(project_dir, 'main.v')
	os.mkdir_all(project_dir)!
	write_template_program(source_path, 'page')!
	os.write_file(page_path, 'V1\n')!

	mut process := new_watch_process(['watch', 'run', source_path, marker_path])
	mut failure := wait_for_marker(marker_path, 'V1', 'the first run', mut process)
	if failure == '' {
		failure = edit_and_wait(page_path, 'V2\n', marker_path, 'V2', mut process)
	}
	output := stop_watch_process(mut process)
	assert failure == '', '${failure}\n${output}'
}

// Unlike the implied filter of veb projects, `--only-watch` is what the user asked for.
fn test_watch_keeps_explicit_only_watch_for_reported_inputs() {
	project_dir := os.join_path(tsource_dir, 'only_watch')
	marker_path := os.join_path(toutput_dir, 'only_watch_marker.txt')
	page_path := os.join_path(project_dir, 'page.txt')
	source_path := os.join_path(project_dir, 'main.v')
	os.mkdir_all(project_dir)!
	write_template_program(source_path, 'page.txt')!
	os.write_file(page_path, 'V1\n')!

	mut process := new_watch_process(['watch', '--only-watch=*.v', 'run', source_path, marker_path])
	mut failure := wait_for_marker(marker_path, 'V1', 'the first run', mut process)
	mut log := []string{}
	if failure == '' {
		time.sleep(1500 * time.millisecond)
		os.write_file(page_path, 'V2\n')!
		// A wrong reload is logged when it starts, which takes a few scan periods at most.
		time.sleep(2 * time.second)
		drain_process_output(mut process, mut log)
		if log.join('').contains('reload cycle:     1') {
			failure = '`--only-watch=*.v` did not exclude a reported `.txt` file'
		}
	}
	if failure == '' {
		// The filter did not stop the watcher: the next allowed change sees the new template.
		failure = edit_and_wait(source_path, os.read_file(source_path)! + '// edited\n',
			marker_path, 'V2', mut process)
	}
	output := log.join('') + stop_watch_process(mut process)
	assert failure == '', '${failure}\n${output}'
}

fn write_template_program(source_path string, template string) ! {
	os.write_file(source_path, "import os

fn page() string {
	return \$tmpl('${template}')
}

fn main() {
	os.write_file(os.args[1], page().trim_space()) or { panic(err) }
}
")!
}

fn new_watch_process(args []string) &os.Process {
	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_redirect_stdio()
	process.use_pgroup = true
	process.set_args(args)
	process.run()
	return process
}

fn stop_watch_process(mut process os.Process) string {
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()
	return output
}

// edit_and_wait changes a watched file, and returns what went wrong if the program
// does not write `expected` to its marker after that.
fn edit_and_wait(path string, content string, marker_path string, expected string, mut process os.Process) string {
	// os.file_last_mod_unix has a 1 second resolution, so the edit has to land in a later second.
	time.sleep(1500 * time.millisecond)
	os.write_file(path, content) or { return err.msg() }
	return wait_for_marker(marker_path, expected, 'the edit of `${os.file_name(path)}`', mut
		process)
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
