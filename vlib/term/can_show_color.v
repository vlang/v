module term

pub fn can_show_color_on_stdout() bool {
	return can_show_color_on_fd(1)
}

pub fn can_show_color_on_stderr() bool {
	return can_show_color_on_fd(2)
}
