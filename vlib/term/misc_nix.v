module term

#include <sys/ioctl.h>

pub struct C.winsize {
pub:
	ws_row u16
	ws_col u16
  ws_xpixel u16
  ws_ypixel u16
}

fn C.ioctl() int

pub fn get_terminal_size() (int, int) {
	w := C.winsize{}
	C.ioctl(0, C.TIOCGWINSZ, &w)
  eprintln('ws_row: $w.ws_row | ws_col: $w.ws_col | ws_xpixel: $w.ws_xpixel | ws_ypixel: $w.ws_ypixel')
	return int(w.ws_col), int(w.ws_row)
}
