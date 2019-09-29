module term

import os

fn C.isatty(int) int

pub fn can_show_color() bool {
	if os.getenv('TERM') == 'dumb' { return false }
	if C.isatty(1) != 0 { return true }
	return false
}
