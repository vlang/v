module vshare

import os
import time

fn test_copy_to_clipboard_with_commands_returns_false_without_candidates() {
	assert copy_to_clipboard_with_commands('https://play.vlang.io/p/test', []) == false
}

fn test_copy_to_clipboard_with_commands_uses_the_first_working_command() {
	test_dir := os.join_path(os.vtmp_dir(), 'vshare_test_${os.getpid()}_${time.now().unix_micro()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	output_path := os.join_path(test_dir, 'clipboard.out')
	fake_clip := os.join_path(test_dir, fake_clipboard_executable_name())
	os.write_file(fake_clip, fake_clipboard_script())!
	$if !windows {
		os.chmod(fake_clip, 0o700)!
	}
	original_path := os.getenv('PATH')
	original_output_path := os.getenv('VSHARE_TEST_OUTPUT')
	new_path := if original_path.len == 0 {
		test_dir
	} else {
		'${test_dir}${os.path_delimiter}${original_path}'
	}
	os.setenv('PATH', new_path, true)
	os.setenv('VSHARE_TEST_OUTPUT', output_path, true)
	defer {
		os.setenv('PATH', original_path, true)
		if original_output_path.len == 0 {
			os.unsetenv('VSHARE_TEST_OUTPUT')
		} else {
			os.setenv('VSHARE_TEST_OUTPUT', original_output_path, true)
		}
	}
	assert copy_to_clipboard_with_commands('https://play.vlang.io/p/test', [
		ClipboardCommand{
			executable: 'missing-clipboard-command'
			command:    'missing-clipboard-command < @FILE@'
		},
		ClipboardCommand{
			executable: fake_clipboard_executable_name()
			command:    fake_clipboard_command()
		},
	])
	output := os.read_file(output_path)!
	$if windows {
		assert output.trim_right('\r\n') == 'https://play.vlang.io/p/test'
	} $else {
		assert output == 'https://play.vlang.io/p/test'
	}
}

fn fake_clipboard_command() string {
	return '${fake_clipboard_executable_name()} < @FILE@'
}

fn fake_clipboard_executable_name() string {
	$if windows {
		return 'fakeclip.bat'
	} $else {
		return 'fakeclip'
	}
}

fn fake_clipboard_script() string {
	$if windows {
		return '@echo off\r\nmore > "%VSHARE_TEST_OUTPUT%"\r\n'
	} $else {
		return '#!/bin/sh\ncat > "\$VSHARE_TEST_OUTPUT"\n'
	}
}
