module term

#include <sys/ioctl.h>

struct C.winsize{
	pub:
	ws_row int
	ws_col int
}

fn C.ioctl() int

pub fn get_term_size() (int, int) {
	// TODO: check for resize events

	mut w := C.winsize{}
	C.ioctl(0, C.TIOCGWINSZ, &w)

	return w.ws_col, w.ws_row
}
