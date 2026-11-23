import os
import time

const vwatch_stdin_vexe = @VEXE
const vwatch_terminal_poll_interval = 20 * time.millisecond
const vwatch_terminal_probe_dir = os.join_path(os.vtmp_dir(), 'vwatch_tty_probe_${os.getpid()}')
const vwatch_terminal_probe_source = r'
module main

import os
import v.util.vwatchtty

#include <unistd.h>

fn C.tcgetpgrp(fd int) int

fn main() {
	mode, target, marker := os.args[1], os.args[2], os.args[3]
	match mode {
		"handoff" {
			mut child := os.new_process(target)
			child.use_pgroup = true
			child.set_args(["10"])
			child.run()
			watcher_pgid := vwatchtty.process_group()
			handed_off := vwatchtty.set_foreground_process_group(child.pid, watcher_pgid)
			worker_pgid := vwatchtty.process_group()
			child.signal_term()
			child.wait()
			child.close()
			os.write_file(marker, "${handed_off},${watcher_pgid},${worker_pgid},${child.pid}")!
		}
		"restore" {
			before := C.tcgetpgrp(0)
			manager_pgid := target.int()
			vwatchtty.restore_foreground_process_group(manager_pgid)
			after := C.tcgetpgrp(0)
			os.write_file(marker, "${before},${after},${manager_pgid}")!
		}
		else { panic("unknown terminal probe mode: ${mode}") }
	}
}
'

fn testsuite_begin() {
	os.rmdir_all(vwatch_terminal_probe_dir) or {}
}

fn testsuite_end() {
	os.rmdir_all(vwatch_terminal_probe_dir) or {}
}

fn build_vwatch_terminal_probe() !string {
	helper := os.join_path(vwatch_terminal_probe_dir, 'vwatch_terminal_probe')
	if os.is_file(helper) {
		return helper
	}
	os.mkdir_all(vwatch_terminal_probe_dir)!
	source := os.join_path(vwatch_terminal_probe_dir, 'vwatch_terminal_probe.c.v')
	os.write_file(source, vwatch_terminal_probe_source)!
	result := os.exec([vwatch_stdin_vexe, '-o', helper, source])
	assert result.exit_code == 0, result.output
	return helper
}

fn finish_vwatch_terminal(mut process os.Process) string {
	sw := time.new_stopwatch()
	for process.is_alive() && sw.elapsed() < 5 * time.second {
		time.sleep(vwatch_terminal_poll_interval)
	}
	if process.is_alive() {
		process.signal_pgkill()
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()
	return output
}

fn openbsd_script_args(stty_exe string, command string, ready_path string) []string {
	// With redirected stdin, OpenBSD `script` initializes its pseudo-terminal
	// at B0. Set a usable speed together with sane modes to avoid hanging it up.
	mut setup_command := '${os.quoted_path(stty_exe)} sane 115200'
	if ready_path != '' {
		setup_command += ' && : > ${os.quoted_path(ready_path)}'
	}
	return ['-c', '${setup_command} && exec ${command}', '/dev/null']
}

fn required_openbsd_tool(name string) string {
	return os.find_abs_path_of_executable(name) or {
		panic('`${name}` is required for OpenBSD vwatch terminal tests')
	}
}

fn test_openbsd_script_args() {
	$if windows {
		return
	}
	command := "'/bin/sh' -m '/tmp/run.sh' '/tmp/helper'"
	assert openbsd_script_args('/bin/stty', command, '') == [
		'-c',
		"'/bin/stty' sane 115200 && exec ${command}",
		'/dev/null',
	]
	assert openbsd_script_args('/bin/stty', command, '/tmp/ready') == [
		'-c',
		"'/bin/stty' sane 115200 && : > '/tmp/ready' && exec ${command}",
		'/dev/null',
	]
}

fn test_background_watch_does_not_take_terminal() {
	$if windows {
		return
	} $else {
		script_exe := os.find_abs_path_of_executable('script') or { return }
		shell_exe := os.find_abs_path_of_executable('sh') or { return }
		tmp_dir := os.join_path(os.vtmp_dir(), 'vwatch_background_tty_${os.getpid()}')
		os.mkdir_all(tmp_dir)!
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		marker_path := os.join_path(tmp_dir, 'handed_off.txt')
		shell_path := os.join_path(tmp_dir, 'run_background.sh')
		sleep_exe := os.find_abs_path_of_executable('sleep') or { return }
		helper_path := build_vwatch_terminal_probe()!
		os.write_file(shell_path, ['set -m', r'"$1" handoff "$2" "$3" &', 'wait'].join('\n'))!

		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		command := '${os.quoted_path(shell_exe)} -m ${os.quoted_path(shell_path)} ${os.quoted_path(helper_path)} ${os.quoted_path(sleep_exe)} ${os.quoted_path(marker_path)}'
		$if openbsd {
			stty_exe := required_openbsd_tool('stty')
			process.set_args(openbsd_script_args(stty_exe, command, ''))
		} $else $if macos || freebsd {
			process.set_args(['-q', '/dev/null', shell_exe, '-m', shell_path, helper_path, sleep_exe,
				marker_path])
		} $else {
			process.set_args(['-q', '-c', command, '/dev/null'])
		}
		process.run()
		output := finish_vwatch_terminal(mut process)
		groups := (os.read_file(marker_path) or { '' }).split(',')
		assert groups.len == 4, output
		assert groups[0] == 'false', output
		assert groups[1] != groups[2], output
		assert groups[2] == groups[3], output
	}
}

fn test_background_restore_does_not_reclaim_terminal() {
	$if windows {
		return
	} $else {
		script_exe := os.find_abs_path_of_executable('script') or { return }
		shell_exe := os.find_abs_path_of_executable('sh') or { return }
		tmp_dir := os.join_path(os.vtmp_dir(), 'vwatch_background_restore_${os.getpid()}')
		os.mkdir_all(tmp_dir)!
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		marker_path := os.join_path(tmp_dir, 'terminal_groups.txt')
		shell_path := os.join_path(tmp_dir, 'run_background_restore.sh')
		helper_path := build_vwatch_terminal_probe()!
		os.write_file(shell_path, ['set -m', 'sleep 10 &', 'manager=$!',
			r'"$1" restore "$manager" "$2" &', 'worker=$!', 'wait "$worker"', 'kill "$manager"',
			'wait "$manager" || true'].join('\n'))!

		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		command := '${os.quoted_path(shell_exe)} -m ${os.quoted_path(shell_path)} ${os.quoted_path(helper_path)} ${os.quoted_path(marker_path)}'
		$if openbsd {
			stty_exe := required_openbsd_tool('stty')
			process.set_args(openbsd_script_args(stty_exe, command, ''))
		} $else $if macos || freebsd {
			process.set_args(['-q', '/dev/null', shell_exe, '-m', shell_path, helper_path, marker_path])
		} $else {
			process.set_args(['-q', '-c', command, '/dev/null'])
		}
		process.run()
		output := finish_vwatch_terminal(mut process)
		groups := (os.read_file(marker_path) or { '' }).split(',')
		assert groups.len == 3, output
		assert groups[0] == groups[1], output
		assert groups[1] != groups[2], output
	}
}

fn test_watch_run_forwards_terminal_input() {
	$if windows {
		return
	} $else {
		script_exe := os.find_abs_path_of_executable('script') or { return }
		tmp_dir := os.join_path(os.vtmp_dir(), 'vwatch_stdin_${os.getpid()}')
		os.mkdir_all(tmp_dir)!
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		source_path := os.join_path(tmp_dir, 'input.v')
		marker_path := os.join_path(tmp_dir, 'input.txt')
		ready_path := os.join_path(tmp_dir, 'pty_ready')
		os.write_file(source_path, "import os\n\nfn main() {\n\tname := os.input('Enter name: ')\n\tos.write_file(os.args[1], name)!\n}\n")!
		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		command := '${os.quoted_path(vwatch_stdin_vexe)} watch --only-watch=*.v run ${os.quoted_path(source_path)} ${os.quoted_path(marker_path)}'
		$if openbsd {
			stty_exe := required_openbsd_tool('stty')
			process.set_args(openbsd_script_args(stty_exe, command, ready_path))
		} $else $if macos || freebsd {
			process.set_args(['-q', '/dev/null', vwatch_stdin_vexe, 'watch', '--only-watch=*.v',
				'run', source_path, marker_path])
		} $else {
			process.set_args(['-q', '-c', command, '/dev/null'])
		}
		process.run()
		mut pty_ready := $if openbsd { false } $else { true }
		mut prompt_ready := false
		mut startup_output := ''
		// Compiling the watched program can take longer on a busy runner.
		// Wait for its prompt before typing or starting the response deadline.
		startup := time.new_stopwatch()
		for startup.elapsed() < time.minute {
			startup_output += process.stdout_read()
			$if openbsd {
				pty_ready = os.exists(ready_path)
			}
			prompt_ready = startup_output.contains('Enter name:')
			if pty_ready && prompt_ready {
				break
			}
			if !process.is_alive() {
				startup_output += process.stdout_read()
				prompt_ready = startup_output.contains('Enter name:')
				break
			}
			time.sleep(vwatch_terminal_poll_interval)
		}
		if pty_ready && prompt_ready && process.is_alive() {
			process.stdin_write('Alice\n')
		}
		mut received_input := false
		response := time.new_stopwatch()
		for response.elapsed() < 10 * time.second {
			if os.read_file(marker_path) or { '' } == 'Alice' {
				received_input = true
				break
			}
			if !process.is_alive() {
				break
			}
			time.sleep(vwatch_terminal_poll_interval)
		}
		if process.is_alive() {
			process.stdin_write('\x03')
		}
		output := startup_output + finish_vwatch_terminal(mut process)
		assert pty_ready, output
		assert prompt_ready, output
		assert received_input, output
	}
}
