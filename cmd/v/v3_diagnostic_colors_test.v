module main

import os
import v.ansi

const replay_color_probe_env = 'VTEST_V3_REPLAY_COLOR_PROBE'

fn testsuite_begin() {
	if os.getenv(replay_color_probe_env) == '1' {
		print(os.getenv('VCOLORS'))
		exit(0)
	}
}

fn test_replay_forwards_the_parent_terminal_color_decision() {
	old_colors := os.getenv_opt('VCOLORS')
	old_probe := os.getenv_opt(replay_color_probe_env)
	defer {
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
		if value := old_probe {
			os.setenv(replay_color_probe_env, value, true)
		} else {
			os.unsetenv(replay_color_probe_env)
		}
	}
	os.unsetenv('VCOLORS')
	os.setenv(replay_color_probe_env, '1', true)
	expected := if ansi.stderr_supports_escape_sequences() { 'always' } else { 'never' }
	assert v3_diagnostics_output(os.executable(), []) == expected
	assert os.getenv('VCOLORS') == ''
}

fn test_replayed_duplicate_diagnostics_respect_color_flags_and_environment() {
	dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	assert os.is_executable(dispatcher), dispatcher
	path := os.join_path(os.vtmp_dir(), 'v3_replay_colors_${os.getpid()}.v')
	os.write_file(path, 'fn main() {}\nfn lighten() {}\nfn lighten() {}\n')!
	old_colors := os.getenv_opt('VCOLORS')
	defer {
		os.rm(path) or {}
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
	}
	for mode in ['always', 'never'] {
		os.setenv('VCOLORS', mode, true)
		for flag in ['', '-color', '-nocolor'] {
			mut args := ['-nocache', '-check']
			if flag != '' {
				args << flag
			}
			args << path
			output := v3_diagnostics_output(dispatcher, args)
			assert output.contains('redefinition of function `lighten`'), output
			assert output.count('conflicting declaration:') == 2, output
			colored := flag == '-color' || (flag == '' && mode == 'always')
			if colored {
				assert output.contains('\x1b[1m\x1b[31mbuilder error:\x1b[39m\x1b[22m'), output
				assert output.contains('\x1b[1m\x1b[35mconflicting declaration:\x1b[39m\x1b[22m'), output
			} else {
				assert !output.contains('\x1b['), output
			}
			assert os.getenv('VCOLORS') == mode
		}
	}
}
