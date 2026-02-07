module veb

#include <sys/sendfile.h>

fn C.sendfile(out_fd i32, in_fd i32, offset voidptr, count i32) i32

fn sendfile(out_fd int, in_fd int, nr_bytes int) int {
	// always pass nil as offset, so the file offset will be used and updated.
	return C.sendfile(out_fd, in_fd, 0, nr_bytes)
}
