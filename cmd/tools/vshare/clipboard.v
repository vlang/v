module vshare

import os
import time

struct ClipboardCommand {
	executable string
	command    string
}

// copy_to_clipboard copies `text` to the first available OS clipboard command.
pub fn copy_to_clipboard(text string) bool {
	return copy_to_clipboard_with_commands(text, clipboard_commands())
}

fn copy_to_clipboard_with_commands(text string, commands []ClipboardCommand) bool {
	if text.len == 0 || commands.len == 0 {
		return false
	}
	temp_file := os.join_path(os.vtmp_dir(), 'vshare_clipboard_${os.getpid()}_${time.now().unix_micro()}.txt')
	os.write_file(temp_file, text) or { return false }
	defer {
		os.rm(temp_file) or {}
	}
	for command in commands {
		if !os.exists_in_system_path(command.executable) {
			continue
		}
		cmd := command.command.replace('@FILE@', os.quoted_path(temp_file))
		if os.execute(cmd).exit_code == 0 {
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
				command:    'type @FILE@ | clip'
			},
		]
	} $else $if macos {
		return [
			ClipboardCommand{
				executable: 'pbcopy'
				command:    'pbcopy < @FILE@'
			},
		]
	} $else {
		return [
			ClipboardCommand{
				executable: 'wl-copy'
				command:    'wl-copy < @FILE@'
			},
			ClipboardCommand{
				executable: 'xclip'
				command:    'xclip -selection clipboard @FILE@'
			},
			ClipboardCommand{
				executable: 'xsel'
				command:    'xsel --clipboard --input < @FILE@'
			},
		]
	}
}
