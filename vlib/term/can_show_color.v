module term

import os

pub fn can_show_color_on_stdout() bool {
	return supports_escape_sequences(1)
}

pub fn can_show_color_on_stderr() bool {
	return supports_escape_sequences(2)
}

fn supports_escape_sequences(fd int) bool {
	$if windows {
		return (is_atty(fd) & 0x0004) > 0 && os.getenv('TERM') != 'dumb' // ENABLE_VIRTUAL_TERMINAL_PROCESSING
	} $else {
		return is_atty(fd) > 0 && os.getenv('TERM') != 'dumb'
	}
}

//////////////////////////////////////////////

pub fn ok_message(s string) string {
	return if can_show_color_on_stdout() {
		green( s )
	}else{
		s
	}
}

pub fn fail_message(s string) string {
	return if can_show_color_on_stdout() {
		red( s )
	}else{
		s
	}
}
