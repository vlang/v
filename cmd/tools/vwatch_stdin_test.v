import os
import time

const vwatch_stdin_vexe = @VEXE

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
		source_path := os.join_path(tmp_dir, 'background_tty.v')
		helper_path := os.join_path(tmp_dir, 'background_tty')
		marker_path := os.join_path(tmp_dir, 'handed_off.txt')
		shell_path := os.join_path(tmp_dir, 'run_background.sh')
		sleep_exe := os.find_abs_path_of_executable('sleep') or { return }
		os.write_file(source_path,
			"import os\nimport v.util.vwatchtty\n\nfn main() {\n\tmut child := os.new_process(os.args[1])\n\tchild.use_pgroup = true\n\tchild.set_args(['10'])\n\tchild.run()\n\twatcher_pgid := vwatchtty.process_group()\n\thanded_off := vwatchtty.set_foreground_process_group(child.pid, watcher_pgid)\n\tworker_pgid := vwatchtty.process_group()\n\tchild.signal_term()\n\tchild.wait()\n\tchild.close()\n\tos.write_file(os.args[2], '\${handed_off},\${watcher_pgid},\${worker_pgid},\${child.pid}')!\n}\n")!
		build_result :=
			os.execute('${os.quoted_path(vwatch_stdin_vexe)} -o ${os.quoted_path(helper_path)} ${os.quoted_path(source_path)}')
		assert build_result.exit_code == 0, build_result.output
		os.write_file(shell_path, ['set -m', r'"$1" "$2" "$3" &', 'wait'].join('\n'))!

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
		for _ in 0 .. 50 {
			if !process.is_alive() {
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
		source_path := os.join_path(tmp_dir, 'restore_tty.c.v')
		helper_path := os.join_path(tmp_dir, 'restore_tty')
		marker_path := os.join_path(tmp_dir, 'terminal_groups.txt')
		shell_path := os.join_path(tmp_dir, 'run_background_restore.sh')
		os.write_file(source_path,
			"module main\n\nimport os\nimport v.util.vwatchtty\n\n#include <unistd.h>\n\nfn C.tcgetpgrp(fd int) int\n\nfn main() {\n\tbefore := C.tcgetpgrp(0)\n\tmanager_pgid := os.args[1].int()\n\tvwatchtty.restore_foreground_process_group(manager_pgid)\n\tafter := C.tcgetpgrp(0)\n\tos.write_file(os.args[2], '\${before},\${after},\${manager_pgid}')!\n}\n")!
		build_result :=
			os.execute('${os.quoted_path(vwatch_stdin_vexe)} -o ${os.quoted_path(helper_path)} ${os.quoted_path(source_path)}')
		assert build_result.exit_code == 0, build_result.output
		os.write_file(shell_path, ['set -m', 'sleep 10 &', 'manager=$!', r'"$1" "$manager" "$2" &',
			'worker=$!', 'wait "$worker"', 'kill "$manager"', 'wait "$manager" || true'].join('\n'))!

		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		command := '${os.quoted_path(shell_exe)} -m ${os.quoted_path(shell_path)} ${os.quoted_path(helper_path)} ${os.quoted_path(marker_path)}'
		$if openbsd {
			stty_exe := required_openbsd_tool('stty')
			process.set_args(openbsd_script_args(stty_exe, command, ''))
		} $else $if macos || freebsd {
			process.set_args(['-q', '/dev/null', shell_exe, '-m', shell_path, helper_path,
				marker_path])
		} $else {
			process.set_args(['-q', '-c', command, '/dev/null'])
		}
		process.run()
		for _ in 0 .. 50 {
			if !process.is_alive() {
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
		os.write_file(source_path,
			"import os\n\nfn main() {\n\tname := os.input('Enter name: ')\n\tos.write_file(os.args[1], name)!\n}\n")!
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
		mut pty_ready := true
		mut prompt_ready := true
		mut startup_output := ''
		$if openbsd {
			pty_ready = false
			prompt_ready = false
			for _ in 0 .. 100 {
				startup_output += process.stdout_read()
				if os.exists(ready_path) {
					pty_ready = true
				}
				if startup_output.contains('Enter name:') {
					prompt_ready = true
				}
				if pty_ready && prompt_ready {
					break
				}
				if !process.is_alive() {
					startup_output += process.stdout_read()
					break
				}
				time.sleep(100 * time.millisecond)
			}
		}
		if pty_ready && prompt_ready && process.is_alive() {
			process.stdin_write('Alice\n')
		}
		mut received_input := false
		for _ in 0 .. 100 {
			if os.read_file(marker_path) or { '' } == 'Alice' {
				received_input = true
				break
			}
			if !process.is_alive() {
				break
			}
			time.sleep(100 * time.millisecond)
		}
		if process.is_alive() {
			process.stdin_write('\x03')
		}
		for _ in 0 .. 50 {
			if !process.is_alive() {
				break
			}
			time.sleep(100 * time.millisecond)
		}
		if process.is_alive() {
			process.signal_pgkill()
		}
		process.wait()
		output := startup_output + process.stdout_slurp() + process.stderr_slurp()
		process.close()
		assert pty_ready, output
		assert prompt_ready, output
		assert received_input, output
	}
}
