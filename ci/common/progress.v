module common

import crypto.sha256
import os

// exec_with_progress checkpoints a successful command and its output files when
// invoked by the aggregate CI runner. Missing or changed outputs force a rerun.
pub fn exec_with_progress(command string, outputs []string) {
	dir := os.getenv('V_MACOS_CI_TASK_PROGRESS')
	if dir == '' {
		exec(command)
		return
	}
	path := os.join_path(dir, 'command-${sha256.hexhash(command)}.ok')
	if os.exists(path) && outputs.all(os.is_file(it)) {
		saved := os.read_file(path) or { panic(err) }
		current := command_progress_contents(command, outputs) or { panic(err) }
		if saved == current {
			eprintln('Skipping completed CI command: ${command}')
			return
		}
	}
	// Never leave a stale success record behind when retrying a changed command.
	if os.exists(path) {
		os.rm(path) or { panic(err) }
	}
	exec(command)
	contents := command_progress_contents(command, outputs) or { panic(err) }
	os.mkdir_all(dir) or { panic(err) }
	tmp_dir := '${path}.${os.getpid()}.tmp'
	os.mkdir(tmp_dir, mode: 0o700) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_path := os.join_path(tmp_dir, 'progress')
	os.write_file(tmp_path, contents) or { panic(err) }
	os.rename(tmp_path, path) or { panic(err) }
}

fn command_progress_contents(command string, outputs []string) !string {
	mut contents := ['macos-ci-command-v1', resolve_v_command(command), os.getenv('VFLAGS')]
	for output in outputs {
		contents << os.real_path(output)
		contents << sha256.hexhash(os.read_file(output)!)
	}
	return contents.str()
}
