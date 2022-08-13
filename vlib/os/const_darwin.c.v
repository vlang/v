module os

// File modes
const (
	o_binary   = 0x00000000 // input and output is not translated; the default on unix
	o_rdonly   = 0x0000 // open the file read-only.
	o_wronly   = 0x0001 // open the file write-only.
	o_rdwr     = 0x0002 // open the file read-write.
	o_nonblock = 0x0004 // prevents blocking when opening files
	o_append   = 0x00000008 // append data to the file when writing.
	o_sync     = 0x0080 // open for synchronous I/O.
	//
	o_nofollow = 0x00000100 // create a new file if none exists.
	o_create   = 0x00000200 // create a new file if none exists.
	o_trunc    = 0x00000400 // truncate regular writable file when opened.
	o_excl     = 0x00000800 // used with o_create, file must not exist.
	//
	o_noctty   = 0x00020000 // if file is terminal, don't make it the controller terminal
)
