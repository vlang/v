import os
import time

const vexe = os.getenv('VEXE')
const vroot = os.dir(vexe)
const tfolder = os.join_path(os.vtmp_dir(), 'os_process_tests')
const test_os_process = os.join_path(tfolder, 'test_os_process.exe')
const test_os_process_source = os.join_path(vroot, 'cmd/tools/test_os_process.v')
const echo_process_exe_filename = os.join_path(tfolder, 'echo.exe')
const echo_process_source_filename = os.join_path(tfolder, 'echo.v')
const delayed_output_exe_filename = os.join_path(tfolder, 'delayed_output.exe')
const delayed_output_source_filename = os.join_path(tfolder, 'delayed_output.v')
const utf16le_output_exe_filename = os.join_path(tfolder, 'utf16le_output.exe')
const utf16le_output_source_filename = os.join_path(tfolder, 'utf16le_output.v')
const echo_process_source_code = '
module main
import io
import os

fn main() {
	unbuffer_stdout()
	mut reader := io.new_buffered_reader(reader: os.stdin(), cap: 1)
	for {
		line := reader.read_line()!
		println(line)
		eprintln(line)
	}
}
'

const delayed_output_source_code = '
module main
import time

fn main() {
	unbuffer_stdout()
	time.sleep(500 * time.millisecond)
	println("late")
	time.sleep(300 * time.millisecond)
}
'

const utf16le_output_source_code = '
module main
import os

fn main() {
	payload := [u8(`O`), 0, `K`, 0, u8(10), 0]
	mut out := os.stdout()
	out.write(payload) or { panic(err) }
}
'

const echo_wait_timeout = 5 // seconds

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder)!
	if os.getenv('WINE_TEST_OS_PROCESS_EXE') != '' {
		// Make it easier to run the test under wine emulation, by just
		// prebuilding the executable with:
		//   v -os windows -o x.exe cmd/tools/test_os_process.v
		//   WINE_TEST_OS_PROCESS_EXE=x.exe ./v -os windows vlib/os/process_test.v
		os.cp(os.getenv('WINE_TEST_OS_PROCESS_EXE'), test_os_process)!
	} else {
		os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(test_os_process)} ${os.quoted_path(test_os_process_source)}')
	}
	assert os.exists(test_os_process)

	os.write_file(echo_process_source_filename, echo_process_source_code)!
	os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(echo_process_exe_filename)} ${os.quoted_path(echo_process_source_filename)}')
	assert os.exists(echo_process_exe_filename)

	os.write_file(delayed_output_source_filename, delayed_output_source_code)!
	os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(delayed_output_exe_filename)} ${os.quoted_path(delayed_output_source_filename)}')
	assert os.exists(delayed_output_exe_filename)

	os.write_file(utf16le_output_source_filename, utf16le_output_source_code)!
	os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(utf16le_output_exe_filename)} ${os.quoted_path(utf16le_output_source_filename)}')
	assert os.exists(utf16le_output_exe_filename)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_getpid() {
	eprintln(@FN)
	pid := os.getpid()
	eprintln('current pid: ${pid}')
	assert pid != 0
}

fn test_set_work_folder() {
	eprintln(@FN)
	new_work_folder := os.real_path(os.temp_dir())
	parent_working_folder := os.getwd()
	dump(new_work_folder)
	dump(parent_working_folder)
	if new_work_folder == parent_working_folder {
		eprintln('... skipping ${@METHOD} because the working folder is the temporary one')
		return
	}
	mut p := os.new_process(test_os_process)
	p.set_args(['-show_wd', '-target', 'stdout'])
	p.set_work_folder(new_work_folder)
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	output := p.stdout_slurp().trim_space()
	p.close()
	$if trace_process_output ? {
		eprintln('p output: "${output}"')
	}
	child_work_folder := output.find_between('stdout, WORK_DIR=', '\n').trim_space()
	dump(child_work_folder)
	assert child_work_folder == new_work_folder
	new_parent_work_folder := os.getwd()
	dump(new_parent_work_folder)
	assert new_parent_work_folder == parent_working_folder
	assert new_parent_work_folder != child_work_folder
}

fn test_set_environment() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	p.set_args(['-show_env', '-target', 'stdout'])
	p.set_environment({
		'V_OS_TEST_PORT': '1234567890'
	})
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	output := p.stdout_slurp().trim_space()
	p.close()
	$if trace_process_output ? {
		eprintln('p output: "${output}"')
	}
	assert output.contains('V_OS_TEST_PORT=1234567890'), output
}

fn test_new_process_uses_exact_executable_path_when_folder_contains_spaces() {
	$if !windows {
		return
	}
	eprintln(@FN)
	spaced_dir := os.join_path(tfolder, 'spawn path with spaces')
	os.rmdir_all(spaced_dir) or {}
	os.mkdir_all(spaced_dir)!
	spaced_exe := os.join_path(spaced_dir, 'test os process.exe')
	os.cp(test_os_process, spaced_exe)!

	stale_source := os.join_path(tfolder, 'spawn.v')
	stale_exe := os.join_path(tfolder, 'spawn.exe')
	os.write_file(stale_source, 'fn main() {\n\tprintln("stale-prefix-exe")\n}\n')!
	assert os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(stale_exe)} ${os.quoted_path(stale_source)}') == 0

	mut p := os.new_process(spaced_exe)
	p.set_args(['-show_env', '-target', 'stdout'])
	p.set_environment({
		'V_OS_TEST_PORT': 'exact_path'
	})
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	output := p.stdout_slurp().trim_space()
	errors := p.stderr_slurp().trim_space()
	p.close()
	assert output.contains('V_OS_TEST_PORT=exact_path'), 'stdout:\n${output}\nstderr:\n${errors}'
	assert !output.contains('stale-prefix-exe'), output
}

fn test_run() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '150', '-period_ms', '50'])
	p.run()
	assert p.status == .running
	assert p.pid > 0
	assert p.pid != os.getpid()
	mut i := 0
	for {
		if !p.is_alive() {
			break
		}
		$if trace_process_output ? {
			os.system('ps -opid= -oppid= -ouser= -onice= -of= -ovsz= -orss= -otime= -oargs= -p ${p.pid}')
		}
		time.sleep(50 * time.millisecond)
		i++
	}
	p.wait()
	assert p.code == 0
	assert p.status == .exited

	eprintln('polling iterations: ${i}')
	assert i < 50
	p.close()
}

fn test_wait() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	assert p.status != .exited
	p.wait()
	assert p.status == .exited
	assert p.code == 0
	assert p.pid != os.getpid()
	p.close()
}

fn test_slurping_output() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '600', '-period_ms', '50'])
	p.set_redirect_stdio()
	assert p.status != .exited
	p.wait()
	assert p.status == .exited
	assert p.code == 0
	output := p.stdout_slurp().trim_space()
	errors := p.stderr_slurp().trim_space()
	p.close()
	$if trace_process_output ? {
		eprintln('---------------------------')
		eprintln('p output: "${output}"')
		eprintln('p errors: "${errors}"')
		eprintln('---------------------------')
	}
	assert output.contains('stdout, 1'), output
	assert output.contains('stdout, 2'), output
	assert output.contains('stdout, 3'), output
	assert output.contains('stdout, 4'), output
	assert errors.contains('stderr, 1'), errors
	assert errors.contains('stderr, 2'), errors
	assert errors.contains('stderr, 3'), errors
	assert errors.contains('stderr, 4'), errors
}

fn echo(mut p os.Process, echo_string string) {
	// append `\n`, as `echo.exe` use `read_line()`
	p.stdin_write(echo_string + '\n')
	mut got_echo_back := false
	mut echo_back := ''
	mut timeout := 0
	for p.is_alive() && timeout < echo_wait_timeout * 1000 / 50 {
		echo_back = p.stdout_read()
		if echo_back.len > 0 {
			got_echo_back = true
			break
		}
		time.sleep(50 * time.millisecond)
		timeout++
	}
	assert got_echo_back
	assert echo_back.trim_space() == echo_string.trim_space()
}

fn test_stdin_write() {
	eprintln(@FN)
	echo_exe := $if windows { echo_process_exe_filename } $else { os.find_abs_path_of_executable('cat') or {
			'/bin/cat'} }
	mut p := os.new_process(echo_exe)
	p.set_redirect_stdio()
	assert p.status != .exited
	p.run()
	echo(mut p, 'hello')
	echo(mut p, 'world')
	p.signal_kill()
	p.close()
}

fn test_stdout_read_returns_immediately_when_no_data_is_pending() {
	eprintln(@FN)
	mut p := os.new_process(delayed_output_exe_filename)
	p.set_redirect_stdio()
	p.run()
	defer {
		if p.is_alive() {
			p.signal_kill()
		}
		p.close()
	}
	mut sw := time.new_stopwatch()
	output := p.stdout_read()
	elapsed_ms := sw.elapsed().milliseconds()
	assert output == ''
	assert elapsed_ms < 300, 'stdout_read blocked for ${elapsed_ms}ms'
	p.wait()
	assert p.stdout_slurp().contains('late')
}

fn test_pipe_read_while_process_is_alive() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '600', '-period_ms', '50'])
	p.set_redirect_stdio()
	p.run()
	mut stdout_output := ''
	mut stderr_output := ''
	mut timeout := 0
	for p.is_alive() && timeout < echo_wait_timeout * 1000 / 20 {
		if out := p.pipe_read(.stdout) {
			stdout_output += out
		}
		if err := p.pipe_read(.stderr) {
			stderr_output += err
		}
		if stdout_output.len > 0 && stderr_output.len > 0 {
			break
		}
		time.sleep(20 * time.millisecond)
		timeout++
	}
	p.wait()
	p.close()
	assert stdout_output.contains('stdout, start'), stdout_output
	assert stderr_output.contains('stderr, start'), stderr_output
}

fn test_pipe_read_returns_none_after_eof() {
	eprintln(@FN)
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '120', '-period_ms', '50'])
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	_ = p.stdout_slurp()
	_ = p.stderr_slurp()
	assert !p.is_pending(.stdout)
	assert !p.is_pending(.stderr)
	if out := p.pipe_read(.stdout) {
		assert false, 'expected none after stdout EOF, got `${out}`'
	}
	if err := p.pipe_read(.stderr) {
		assert false, 'expected none after stderr EOF, got `${err}`'
	}
	p.close()
}

fn test_slurping_utf16le_output_on_windows() {
	if os.user_os() != 'windows' {
		return
	}
	mut p := os.new_process(utf16le_output_exe_filename)
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	output := p.stdout_slurp()
	errors := p.stderr_slurp()
	p.close()
	assert output == 'OK\n', output
	assert errors == ''
}
