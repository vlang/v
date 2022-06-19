module ui

pub struct C.termios {
mut:
	c_iflag int
	c_oflag int
	c_cflag int
	c_lflag int
	//	c_line  byte
	c_cc [10]int
}
