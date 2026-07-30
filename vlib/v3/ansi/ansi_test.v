module ansi

import os

fn test_color_formatting_and_stderr_override() {
	assert bold('x') == '\x1b[1mx\x1b[22m'
	assert red('x') == '\x1b[31mx\x1b[39m'
	assert yellow('x') == '\x1b[33mx\x1b[39m'
	assert blue('x') == '\x1b[34mx\x1b[39m'
	old_colors := os.getenv_opt('VCOLORS')
	defer {
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
	}
	os.setenv('VCOLORS', 'always', true)
	assert bright_blue_stderr('x') == '\x1b[94mx\x1b[39m'
	os.setenv('VCOLORS', 'never', true)
	assert bright_blue_stderr('x') == 'x'
}
