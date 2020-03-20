module os
// (Must be realized in Syscall) (Must be specified)

// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
const (
	S_IFMT = 0xF000 // type of file
	S_IFDIR = 0x4000 // directory
	S_IFLNK = 0xa000 // link
	S_IXUSR = 0o100  // is executable by the owner
	S_IXGRP = 0o010  // is executable by group
	S_IXOTH = 0o001  // is executable by others
)

const (
	STD_INPUT_HANDLE = -10
	STD_OUTPUT_HANDLE = -11
	STD_ERROR_HANDLE = -12
)

