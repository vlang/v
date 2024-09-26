module veb

#include <sys/sendfile.h>

fn C.sendfile(out_fd int, in_fd int, offset voidptr, count int) int

fn sendfile(out_fd int, in_fd int, nr_bytes int) int {
	// always pass nil as offset, so the file offset will be used and updated.
	return C.sendfile(out_fd, in_fd, 0, nr_bytes)
}
