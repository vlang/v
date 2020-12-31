module os

// (Must be realized in Syscall) (Must be specified)
// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
/*
const (
	s_ifmt  = 0xF000 // type of file
	s_ifdir = 0x4000 // directory
	s_iflnk = 0xa000 // link
	s_isuid = 0o4000 // SUID
	s_isgid = 0o2000 // SGID
	s_isvtx = 0o1000 // Sticky
	s_irusr = 0o0400 // Read by owner
	s_iwusr = 0o0200 // Write by owner
	s_ixusr = 0o0100 // Execute by owner
	s_irgrp = 0o0040 // Read by group
	s_iwgrp = 0o0020 // Write by group
	s_ixgrp = 0o0010 // Execute by group
	s_iroth = 0o0004 // Read by others
	s_iwoth = 0o0002 // Write by others
	s_ixoth = 0o0001 // Execute by others
)
*/
const (
	std_input_handle  = -10
	std_output_handle = -11
	std_error_handle  = -12
)
