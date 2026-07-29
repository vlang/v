module veb

fn C.sendfile(in_fd i32, out_fd i32, offset i64, count usize, hdtr voidptr, sent &i64, flags i32) i32

fn sendfile(out_fd int, in_fd int, nr_bytes int) int {
	// out_fd must be a stream socket descriptor.
	mut sent := i64(0)
	r := C.sendfile(in_fd, out_fd, 0, usize(nr_bytes), unsafe { nil }, &sent, 0)
	if r == 0 {
		return nr_bytes
	}
	return r
}
