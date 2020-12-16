module os

// (Must be realized in Syscall) (Must be specified)
// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
const (
	s_ifmt  = 0xF000 // type of file
	s_ifdir = 0x4000 // directory
	s_iflnk = 0xa000 // link
	s_ixusr = 0o100 // is executable by the owner
	s_ixgrp = 0o010 // is executable by group
	s_ixoth = 0o001 // is executable by others
)

const (
	std_input_handle  = -10
	std_output_handle = -11
	std_error_handle  = -12
)
