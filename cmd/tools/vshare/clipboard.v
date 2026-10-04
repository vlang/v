module vshare

import os
import time

struct ClipboardCommand {
	executable string
	args       []string
}

// copy_to_clipboard copies `text` to the first available OS clipboard command.
pub fn copy_to_clipboard(text string) bool {
	return copy_to_clipboard_with_commands(text, clipboard_commands())
}

fn copy_to_clipboard_with_commands(text string, commands []ClipboardCommand) bool {
	if text.len == 0 || commands.len == 0 {
		return false
	}
	temp_file := os.join_path(os.vtmp_dir(),
		'vshare_clipboard_${os.getpid()}_${time.now().unix_micro()}.txt')
	os.write_file(temp_file, text) or { return false }
	defer {
		os.rm(temp_file) or {}
	}
	for command in commands {
		if !os.exists_in_system_path(command.executable) {
			continue
		}
		mut process := os.new_process(command.executable)
		process.set_args(command.args)
		process.set_stdin_path(temp_file)
		process.wait()
		code := process.code
		process.close()
		if code == 0 {
			return true
		}
	}
	return false
}

fn clipboard_commands() []ClipboardCommand {
	$if windows {
		return [
			ClipboardCommand{
				executable: 'clip.exe'
				args:       []string{}
			},
		]
	} $else $if macos {
		return [
			ClipboardCommand{
				executable: 'pbcopy'
				args:       []string{}
			},
		]
	} $else {
		return [
			ClipboardCommand{
				executable: 'wl-copy'
				args:       []string{}
			},
			ClipboardCommand{
				executable: 'xclip'
				args:       ['-selection', 'clipboard']
			},
			ClipboardCommand{
				executable: 'xsel'
				args:       ['--clipboard', '--input']
			},
		]
	}
}
