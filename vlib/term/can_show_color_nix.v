module term

import os

fn C.isatty(int) int

pub fn can_show_color_on_fd(fd int) bool {
	if os.getenv('TERM') == 'dumb' { return false }
	if C.isatty(fd) != 0 { return true }
	return false
}
