module os

// The o_* values below keep the Linux flag encoding under freestanding;
// freestanding backend has no libc headers, so the constants are literals
// there (Zig: std/os/linux.zig `O`). The numbers are identical for both
// supported architectures.
const max_path_buffer_size = max_path_len

const o_binary = 0 // input and output is not translated; the default on unix

const o_rdonly = $if freestanding || syscalls { 0 } $else { C.O_RDONLY } // open the file read-only.

const o_wronly = $if freestanding || syscalls { 1 } $else { C.O_WRONLY } // open the file write-only.

const o_rdwr = $if freestanding || syscalls { 2 } $else { C.O_RDWR } // open the file read-write.

const o_create = $if freestanding || syscalls { 0o100 } $else { C.O_CREAT } // create a new file if none exists.

const o_excl = $if freestanding || syscalls { 0o200 } $else { C.O_EXCL } // used with o_create, file must not exist.

const o_noctty = $if freestanding || syscalls { 0o400 } $else { C.O_NOCTTY } // if file is terminal, don't make it the controller terminal

const o_trunc = $if freestanding || syscalls { 0o1000 } $else { C.O_TRUNC } // truncate regular writable file when opened.

const o_append = $if freestanding || syscalls { 0o2000 } $else { C.O_APPEND } // append data to the file when writing.

const o_nonblock = $if freestanding || syscalls { 0o4000 } $else { C.O_NONBLOCK } // prevents blocking when opening files

const o_sync = $if freestanding || syscalls { 0o4010000 } $else { C.O_SYNC }
