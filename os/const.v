module os

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
