module os

const max_path_buffer_size = max_path_len

const o_binary = 0 // input and output is not translated; the default on unix

const o_rdonly = C.O_RDONLY // open the file read-only.

const o_wronly = C.O_WRONLY // open the file write-only.

const o_rdwr = C.O_RDWR // open the file read-write.

const o_create = C.O_CREAT // create a new file if none exists.

const o_excl = C.O_EXCL // used with o_create, file must not exist.

const o_noctty = C.O_NOCTTY // if file is terminal, don't make it the controller terminal

const o_trunc = C.O_TRUNC // truncate regular writable file when opened.

const o_append = C.O_APPEND // append data to the file when writing.

const o_nonblock = C.O_NONBLOCK // prevents blocking when opening files

const o_sync = C.O_SYNC
