module main

import os

fn test_trim_symlink_command_accepts_forwarded_subcommand() {
	assert trim_symlink_command([]string{}) == []string{}
	assert trim_symlink_command(['symlink']) == []string{}
	assert trim_symlink_command(['symlink', '~/.local/bin']) == ['~/.local/bin']
	assert trim_symlink_command(['~/.local/bin']) == ['~/.local/bin']
}

fn test_parse_symlink_options_defaults_to_default_location() {
	options := parse_symlink_options([]string{}) or { panic(err) }
	assert options == SymlinkOptions{}
}

fn test_parse_symlink_options_accepts_custom_directory() {
	options := parse_symlink_options(['symlink', '~/.local/bin']) or { panic(err) }
	assert options.github_ci == false
	assert options.link_dir == '~/.local/bin'
}

fn test_parse_symlink_options_supports_githubci() {
	options := parse_symlink_options(['symlink', '-githubci']) or { panic(err) }
	assert options.github_ci
	assert options.link_dir == ''
}

fn test_parse_symlink_options_rejects_multiple_arguments() {
	if options := parse_symlink_options(['symlink', '/tmp/bin', 'extra']) {
		assert false, 'expected an error, got ${options}'
	} else {
		assert err.msg() == symlink_usage
	}
}

fn test_normalized_link_dir_expands_tilde() {
	path := os.join_path('~', '.local', 'bin')
	expected := os.join_path(os.home_dir(), '.local', 'bin')
	assert normalized_link_dir(path) == expected
}

fn test_symlink_path_uses_platform_binary_name() {
	$if windows {
		assert symlink_path(r'C:\tools\vbin') == os.join_path(r'C:\tools\vbin', 'v.exe')
	} $else {
		assert symlink_path('/tmp/vbin') == os.join_path('/tmp/vbin', 'v')
	}
}
