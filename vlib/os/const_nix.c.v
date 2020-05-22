module os

// File modes
const (
	o_rdonly	= 000000000 // open the file read-only.
	o_wronly	= 000000001 // open the file write-only.
	o_rdwr		= 000000002 // open the file read-write.
	o_create	= 000000100 // create a new file if none exists.
	o_excl		= 000000200 // used with o_create, file must not exist.
	o_noctty	= 000000400 // if file is terminal, don't make it the controller terminal
	o_trunc		= 000001000 // truncate regular writable file when opened.
	o_append	= 000002000 // append data to the file when writing.
	o_nonblock	= 000004000 // prevents blocking when opening files
	o_sync		= 000010000 // open for synchronous I/O.
)
