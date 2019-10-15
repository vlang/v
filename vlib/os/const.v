module os

// (Must be realized in Syscall) (Must be specified)
// File modes.

const (
    O_RDONLY = 1 // open the file read-only.
    O_WRONLY = 2 // open the file write-only.
    O_RDWR   = 3 // open the file read-write.

    O_APPEND = 8   // append data to the file when writing.
    O_CREATE = 16  // create a new file if none exists.
    O_EXCL   = 32  // used with O_CREATE, file must not exist.
    O_SYNC   = 64  // open for synchronous I/O.
    O_TRUNC  = 128 // truncate regular writable file when opened.
)

// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
const (
	S_IFMT   = 0xF000 // a mask for C.stat st_mode field, so that later
	                  // comparing with the following constants works
	S_IFIFO  = 0x1000 //   a FIFO pipe
	S_IFCHR  = 0x2000 //   a character device
	S_IFDIR  = 0x4000 // + directory
	S_IFBLK  = 0x6000 //   a block device
	S_IFREG  = 0x8000 // + a regular file
	S_IFLNK  = 0xA000 // + a symbolic link
)

const(
    STD_INPUT_HANDLE  = -10
    STD_OUTPUT_HANDLE = -11
    STD_ERROR_HANDLE  = -12
)



