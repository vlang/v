module veb

fn C.sendfile(in_fd int, out_fd int, offset int, count int, voidptr offsetp, voidptr hdr, flags int) int

fn sendfile(out_fd int, in_fd int, nr_bytes int) int {
	// out_fd must be a stream socket descriptor.
	r := C.sendfile(in_fd, out_fd, 0, nr_bytes, unsafe { nil }, unsafe { nil }, 0)
	if r == 0 {
		return nr_bytes
	}
	return r
}
