module os

// File modes
const (
	o_rdonly   = 0o00000000 // open the file read-only.
	o_wronly   = 0o00000001 // open the file write-only.
	o_rdwr     = 0o00000002 // open the file read-write.
	o_binary   = 0o00000000 // input and output is not translated; the default on unix
	o_create   = 0o00000100 // create a new file if none exists.
	o_excl     = 0o00000200 // used with o_create, file must not exist.
	o_noctty   = 0o00000400 // if file is terminal, don't make it the controller terminal
	o_trunc    = 0o00001000 // truncate regular writable file when opened.
	o_append   = 0o00002000 // append data to the file when writing.
	o_nonblock = 0o00004000 // prevents blocking when opening files
	o_sync     = 0o04010000 // open for synchronous I/O.
)
