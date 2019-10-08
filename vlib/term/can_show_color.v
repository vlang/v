module term

import os

pub fn can_show_color_on_stdout() bool {
	return is_atty(1) && os.getenv('TERM') != 'dumb'
}

pub fn can_show_color_on_stderr() bool {
	return is_atty(2) && os.getenv('TERM') != 'dumb'
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
