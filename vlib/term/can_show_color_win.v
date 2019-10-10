module term

import os

// TODO: implement proper checking on windows too.
// For now, just return false by default
pub fn can_show_color_on_fd(fd int) bool {
	if os.getenv('TERM') == 'dumb' { return false }
	return false
}
