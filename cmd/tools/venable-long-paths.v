module main

import os

const enable_long_paths_help_text = 'Enable the Windows Win32 long-path policy, so the CRT and plain Win32 calls accept paths past MAX_PATH (260 characters). The policy is machine-wide and needs administrator rights, like the Python installer\'s "Disable path length limit" option.

Usage:
  v enable-long-paths [options]

Options:
      --check   Only report the policy, without changing it.
  -h, --help    Display this help message.'

struct EnableLongPathsOptions {
	check     bool
	show_help bool
}

fn parse_enable_long_paths_args(args []string) !EnableLongPathsOptions {
	mut check := false
	mut show_help := false
	for arg in args {
		match arg {
			'-check', '--check' {
				check = true
			}
			'-h', '-help', '--help' {
				show_help = true
			}
			else {
				return error('v enable-long-paths: unknown option `${arg}`')
			}
		}
	}
	return EnableLongPathsOptions{
		check:     check
		show_help: show_help
	}
}

// long_paths_status_line summarizes the policy in one lowercase word.
fn long_paths_status_line(status os.LongPathsStatus) string {
	return match status {
		.enabled { 'enabled' }
		.disabled { 'disabled' }
		.unknown { 'unknown' }
	}
}

// enable_long_paths_denied_message tells how to retry without administrator
// rights, naming the exact elevated command.
fn enable_long_paths_denied_message() string {
	return 'could not enable Win32 long paths: access is denied.\nRun `v enable-long-paths` in an elevated terminal (right-click, Run as administrator), or run:\n${os.long_paths_manual_command()}'
}

fn main() {
	if os.user_os() != 'windows' {
		eprintln('`v enable-long-paths` is only supported on Windows.')
		exit(1)
	}
	args := os.args[1..]
	command_args := if args.len > 0 && args[0] == 'enable-long-paths' {
		args[1..]
	} else {
		args
	}
	opts := parse_enable_long_paths_args(command_args) or {
		eprintln(err.msg())
		eprintln(enable_long_paths_help_text)
		exit(1)
	}
	if opts.show_help {
		println(enable_long_paths_help_text)
		return
	}
	status := os.long_paths_status()
	if opts.check {
		println('Win32 long paths are ${long_paths_status_line(status)}.')
		if status != os.LongPathsStatus.enabled {
			exit(1)
		}
		return
	}
	if status == os.LongPathsStatus.enabled {
		println('Win32 long paths are already enabled.')
		return
	}
	os.enable_long_paths() or {
		if err.code() == 5 {
			eprintln(enable_long_paths_denied_message())
		} else {
			eprintln(err.msg())
		}
		exit(1)
	}
	println('Win32 long paths were enabled. New processes pick it up; if builds still fail past MAX_PATH, restart the terminal (or sign out and back in).')
}
