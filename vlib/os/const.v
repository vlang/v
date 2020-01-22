module os
// (Must be realized in Syscall) (Must be specified)

// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
const (
	S_IFMT = 0xF000 // type of file
	S_IFDIR = 0x4000 // directory
	S_IFLNK = 0xa000 // link
)

const (
	STD_INPUT_HANDLE = -10
	STD_OUTPUT_HANDLE = -11
	STD_ERROR_HANDLE = -12
)

