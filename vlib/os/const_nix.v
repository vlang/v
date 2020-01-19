module os

// File modes
const (
	O_RDONLY	= 000000000	// open the file read-only.
	O_WRONLY	= 000000001	// open the file write-only.
	O_RDWR		= 000000002	// open the file read-write.
	O_CREATE	= 000000100	// create a new file if none exists.
	O_EXCL		= 000000200	// used with O_CREATE, file must not exist.
	O_TRUNC		= 000001000	// truncate regular writable file when opened.
	O_APPEND	= 000002000	// append data to the file when writing.
	O_SYNC		= 000010000	// open for synchronous I/O.
)