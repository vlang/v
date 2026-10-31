module ansi

import os

// `VCOLORS` is the escape hatch, so it must win over whatever the terminal
// reports, in both directions.
fn test_stderr_supports_escape_sequences_honours_the_vcolors_override() {
	previous := os.getenv_opt('VCOLORS')
	defer {
		if value := previous {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
	}
	os.setenv('VCOLORS', 'always', true)
	assert stderr_supports_escape_sequences()
	os.setenv('VCOLORS', 'never', true)
	assert !stderr_supports_escape_sequences()
	// Only those two values are an override; anything else falls through.
	os.setenv('VCOLORS', 'auto', true)
	assert stderr_supports_escape_sequences() == (!is_dumb_terminal() && is_terminal())
	os.unsetenv('VCOLORS')
	assert stderr_supports_escape_sequences() == (!is_dumb_terminal() && is_terminal())
}

// A dumb terminal never gets escapes, whatever the override-free answer would be.
fn test_stderr_supports_escape_sequences_refuses_a_dumb_terminal() {
	previous_colors := os.getenv_opt('VCOLORS')
	previous_term := os.getenv_opt('TERM')
	defer {
		if value := previous_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
		if value := previous_term {
			os.setenv('TERM', value, true)
		} else {
			os.unsetenv('TERM')
		}
	}
	os.unsetenv('VCOLORS')
	os.setenv('TERM', 'dumb', true)
	assert !stderr_supports_escape_sequences()
	// The override still wins over the terminal, so `always` is not suppressed here.
	os.setenv('VCOLORS', 'always', true)
	assert stderr_supports_escape_sequences()
}

fn is_dumb_terminal() bool {
	return os.getenv('TERM') == 'dumb'
}

fn is_terminal() bool {
	$if windows {
		return (os.is_atty(2) & 0x0004) > 0
	} $else {
		return os.is_atty(2) > 0
	}
}
