module os

const max_path_buffer_size = max_path_len

const (
	o_binary   = 0 // input and output is not translated; the default on unix
	o_rdonly   = C.O_RDONLY // open the file read-only.
	o_wronly   = C.O_WRONLY // open the file write-only.
	o_rdwr     = C.O_RDWR // open the file read-write.
	o_create   = C.O_CREAT // create a new file if none exists.
	o_excl     = C.O_EXCL // used with o_create, file must not exist.
	o_noctty   = C.O_NOCTTY // if file is terminal, don't make it the controller terminal
	o_trunc    = C.O_TRUNC // truncate regular writable file when opened.
	o_append   = C.O_APPEND // append data to the file when writing.
	o_nonblock = C.O_NONBLOCK // prevents blocking when opening files
	o_sync     = C.O_SYNC // open for synchronous I/O.
)
