@[has_globals]
module ansi

import os

__global colors_enabled = true

// set_colors_enabled controls ANSI wrapping for compiler diagnostics.
pub fn set_colors_enabled(enabled bool) {
	colors_enabled = enabled
}

@[inline]
fn format(message string, open string, close string) string {
	if !colors_enabled {
		return message
	}
	return '\x1b[${open}m${message}\x1b[${close}m'
}

// bold returns message wrapped in ANSI bold escape sequences.
pub fn bold(message string) string {
	return format(message, '1', '22')
}

// red returns message wrapped in ANSI red escape sequences.
pub fn red(message string) string {
	return format(message, '31', '39')
}

// yellow returns message wrapped in ANSI yellow escape sequences.
pub fn yellow(message string) string {
	return format(message, '33', '39')
}

// blue returns message wrapped in ANSI blue escape sequences.
pub fn blue(message string) string {
	return format(message, '34', '39')
}

// bright_blue_stderr highlights message when stderr supports ANSI colors.
pub fn bright_blue_stderr(message string) string {
	if colors_enabled && stderr_supports_escape_sequences() {
		return format(message, '94', '39')
	}
	return message
}

// stderr_supports_escape_sequences reports whether stderr and the environment permit ANSI colors.
pub fn stderr_supports_escape_sequences() bool {
	override := os.getenv('VCOLORS')
	if override == 'always' {
		return true
	}
	if override == 'never' || os.getenv('TERM') == 'dumb' {
		return false
	}
	$if windows {
		if os.getenv('ConEmuANSI') == 'ON' {
			return true
		}
		return (os.is_atty(2) & 0x0004) > 0
	} $else {
		return os.is_atty(2) > 0
	}
}
