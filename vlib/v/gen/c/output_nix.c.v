module c

import os

#include <sys/uio.h>
#include <unistd.h>

fn write_c_output_sequential(mut file os.File, prefix []u8, segments []string, tail string, separator string) ! {
	if prefix.len > 0 {
		unsafe { file.write_full_buffer(prefix.data, usize(prefix.len))! }
	}
	for segment in segments {
		if segment.len > 0 {
			if separator.len > 0 {
				file.write_string(separator)!
			}
			file.write_string(segment)!
		}
	}
	if tail.len > 0 {
		file.write_string(tail)!
	}
}

// separator is written ahead of every non-empty segment, which is how a split C
// build marks the boundaries its units are cut at.
//
// The pieces are handed to the kernel with batched writev calls, which copy them
// straight into the page cache. A shared mapping of the new file was measured
// slower: every fresh page faults in first, and faulting it from several threads
// only contends on the one file.
fn write_c_output_vectored(path string, prefix []u8, segments []string, tail string, separator string) ! {
	mut iovs := []C.iovec{cap: segments.len * 2 + 2}
	if prefix.len > 0 {
		iovs << C.iovec{
			iov_base: prefix.data
			iov_len:  usize(prefix.len)
		}
	}
	for segment in segments {
		if segment.len > 0 {
			if separator.len > 0 {
				iovs << C.iovec{
					iov_base: separator.str
					iov_len:  usize(separator.len)
				}
			}
			iovs << C.iovec{
				iov_base: segment.str
				iov_len:  usize(segment.len)
			}
		}
	}
	if tail.len > 0 {
		iovs << C.iovec{
			iov_base: tail.str
			iov_len:  usize(tail.len)
		}
	}
	mut file := os.open_file(path, 'wb')!
	defer {
		file.close()
	}
	mut i := 0
	for i < iovs.len {
		count := if iovs.len - i < c_output_iov_batch { iovs.len - i } else { c_output_iov_batch }
		written := C.writev(file.fd, unsafe { &iovs[i] }, count)
		if written <= 0 {
			if written < 0 && C.errno == C.EINTR {
				continue
			}
			return error('failed to write ${path}: ${os.posix_get_error_msg(C.errno)}')
		}
		// A short write stops inside some piece: skip the completed pieces and
		// resume from the unwritten remainder of the partial one.
		mut left := usize(written)
		for left > 0 && i < iovs.len {
			if left >= iovs[i].iov_len {
				left -= iovs[i].iov_len
				i++
			} else {
				iovs[i].iov_base = unsafe { &u8(iovs[i].iov_base) + left }
				iovs[i].iov_len -= left
				left = 0
			}
		}
	}
}

// Stay well below IOV_MAX (1024 on macOS and Linux).
const c_output_iov_batch = 512

struct C.iovec {
mut:
	iov_base voidptr
	iov_len  usize
}

fn C.writev(fd int, iov &C.iovec, iovcnt int) isize
