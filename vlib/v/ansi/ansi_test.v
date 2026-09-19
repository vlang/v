module ansi

import os

fn test_color_formatting_and_stderr_override() {
	set_colors_enabled(true)
	assert bold('x') == '\x1b[1mx\x1b[22m'
	assert red('x') == '\x1b[31mx\x1b[39m'
	assert yellow('x') == '\x1b[33mx\x1b[39m'
	assert blue('x') == '\x1b[34mx\x1b[39m'
	old_colors := os.getenv_opt('VCOLORS')
	defer {
		set_colors_enabled(true)
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
	}
	os.setenv('VCOLORS', 'always', true)
	assert bright_blue_stderr('x') == '\x1b[94mx\x1b[39m'
	set_colors_enabled(false)
	assert bold('x') == 'x'
	assert red('x') == 'x'
	assert yellow('x') == 'x'
	assert blue('x') == 'x'
	assert bright_blue_stderr('x') == 'x'
	set_colors_enabled(true)
	os.setenv('VCOLORS', 'never', true)
	assert bright_blue_stderr('x') == 'x'
}
