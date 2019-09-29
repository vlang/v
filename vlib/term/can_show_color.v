module term

pub fn can_show_color_on_stdout() bool {
	return can_show_color_on_fd(int(C.STDOUT_FILENO))
}

pub fn can_show_color_on_stderr() bool {
	return can_show_color_on_fd(int(C.STDERR_FILENO))
}
