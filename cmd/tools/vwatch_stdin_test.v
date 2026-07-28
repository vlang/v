import os
import time

const vwatch_stdin_vexe = @VEXE

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
		os.write_file(source_path,
			'import os\nimport v.util.vwatchtty\n\nfn main() {\n\thanded_off := vwatchtty.set_foreground_process_group(os.getpid(), vwatchtty.process_group())\n\tos.write_file(os.args[1], handed_off.str())!\n}\n')!
		build_result :=
			os.execute('${os.quoted_path(vwatch_stdin_vexe)} -o ${os.quoted_path(helper_path)} ${os.quoted_path(source_path)}')
		assert build_result.exit_code == 0, build_result.output
		os.write_file(shell_path, ['set -m', r'"$1" "$2" &', 'wait'].join('\n'))!

		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		$if macos || freebsd || openbsd {
			process.set_args(['-q', '/dev/null', shell_exe, '-m', shell_path, helper_path,
				marker_path])
		} $else {
			command := '${os.quoted_path(shell_exe)} -m ${os.quoted_path(shell_path)} ${os.quoted_path(helper_path)} ${os.quoted_path(marker_path)}'
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
		assert os.read_file(marker_path) or { '' } == 'false', output
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
		os.write_file(source_path,
			"import os\n\nfn main() {\n\tname := os.input('Enter name: ')\n\tos.write_file(os.args[1], name)!\n}\n")!
		mut process := os.new_process(script_exe)
		process.set_redirect_stdio()
		process.use_pgroup = true
		$if macos || freebsd || openbsd {
			process.set_args(['-q', '/dev/null', vwatch_stdin_vexe, 'watch', 'run', source_path,
				marker_path])
		} $else {
			command := '${os.quoted_path(vwatch_stdin_vexe)} watch run ${os.quoted_path(source_path)} ${os.quoted_path(marker_path)}'
			process.set_args(['-q', '-c', command, '/dev/null'])
		}
		process.run()
		process.stdin_write('Alice\n')
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
		process.stdin_write('\x03')
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
		assert received_input, output
	}
}
