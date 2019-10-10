module term

pub fn can_show_color_on_stdout() bool {
	return can_show_color_on_fd(1)
}

pub fn can_show_color_on_stderr() bool {
	return can_show_color_on_fd(2)
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
