module os
// (Must be realized in Syscall) (Must be specified)
// File modes.
const (
	O_RDONLY 	= 0 		// open the file read-only.
	O_WRONLY 	= 1 		// open the file write-only.
	O_RDWR 		= 2 		// open the file read-write.
	O_APPEND 	= 0x0008 	// append data to the file when writing.
	O_CREATE 	= 0x0100 	// create a new file if none exists.
	O_TRUNC 	= 0x0200 	// truncate regular writable file when opened.
	O_EXCL 		= 0x0400 	// used with O_CREATE, file must not exist.
	O_SYNC 		= 64 		// open for synchronous I/O.
	O_TEXT		= 0x4000	// CR-LF in file becomes LF in memory
	O_BINARY	= 0x8000	// Input and output are not translated
)
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

