module pkgconfig

import os

const main_test_fixture_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'pkgconfig', 'testdata',
	'static_pkgconfig')

fn main_test_fixture_result(args []string) MainResult {
	old_path := os.getenv_opt('PKG_CONFIG_PATH')
	old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
	os.unsetenv('PKG_CONFIG_PATH')
	os.setenv('PKG_CONFIG_PATH_DEFAULTS', main_test_fixture_dir, true)
	defer {
		if path := old_path {
			os.setenv('PKG_CONFIG_PATH', path, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH')
		}
		if defaults := old_defaults {
			os.setenv('PKG_CONFIG_PATH_DEFAULTS', defaults, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH_DEFAULTS')
		}
	}
	mut command := main(args) or { panic(err) }
	return command.run_result() or { panic(err) }
}

fn assert_main_test_shell_round_trip(output string, expected []string) {
	$if !windows {
		script := 'set -- ' + output + r'; printf "%s\n" "$@"'
		result := os.execute('sh -c ${os.quoted_path(script)}')
		assert result.exit_code == 0, result.output
		assert result.output.split_into_lines() == expected
	}
}

fn test_main_exposes_parsed_mode_and_actions() {
	for args in [
		['--static', 'package'],
		['-s', 'package'],
		['--static=true', 'package'],
	] {
		command := main(args)!
		assert command.link_mode() == .static_, args.str()
		assert !command.has_actions, args.str()
	}

	for args in [
		['-s', '-V', 'package'],
		['-s', '-cD', 'package'],
		['-s', '-lD', 'package'],
		['--static', '--cflags=false', 'package'],
		['--static', '--libs=false', 'package'],
		['--cflags', 'package'],
	] {
		command := main(args)!
		assert command.has_actions, args.str()
	}
}

fn test_checker_defaults_and_static_mode_are_explicit_main_api_operations() {
	mut command := main(['--static', 'package'])!
	assert command.link_mode() == .static_
	assert !command.has_actions
	assert !command.opt.cflags
	assert !command.opt.libs

	command.apply_default_actions()
	assert command.has_actions
	assert command.opt.cflags
	assert command.opt.libs

	mut global_static := main(['package'])!
	assert global_static.link_mode() == .dynamic
	global_static.force_static()
	assert global_static.link_mode() == .static_
}

fn test_static_main_output_shell_escapes_structured_link_flags() {
	static_result := main_test_fixture_result([
		'--static',
		'--libs',
		'quoted-split-link-pairs-74',
	])
	expected_flags := ['-L', '/issue74/public search', '-l', 'issue74_public', '-L',
		'/issue74/private search', '-l', 'issue74_private']
	assert static_result.cflags == []
	assert static_result.link_flags == expected_flags
	assert static_result.output == r'-L /issue74/public\ search -l issue74_public -L /issue74/private\ search -l issue74_private'
	assert_main_test_shell_round_trip(static_result.output, expected_flags)

	special_args := [r'back\slash', 'double"quote', "single'quote"]
	special_output := special_args.map(escape_static_output_arg).join(' ')
	assert special_output == 'back\\\\slash double\\"quote single\\\'quote'
	assert_main_test_shell_round_trip(special_output, special_args)

	dynamic_result := main_test_fixture_result(['--libs', 'quoted-split-link-pairs-74'])
	assert dynamic_result.output == '-L "/issue74/public search" -l issue74_public'
}
