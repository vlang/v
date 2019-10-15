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
	S_IFMT   = 0170000 // mask for C.stat st_mode field
	S_IFSOCK = 0140000 // socket
	S_IFLNK  = 0120000 // symlink
	S_IFREG  = 0100000 // regular file
	S_IFDIR  = 0040000 // directory
	S_IFIFO  = 0010000 // FIFO
)

const(
    STD_INPUT_HANDLE  = -10
    STD_OUTPUT_HANDLE = -11
    STD_ERROR_HANDLE  = -12
)



