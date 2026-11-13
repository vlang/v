module os

$if !freestanding && !syscalls {
	fn test_fd_is_pending_low_pipe_readiness() ! {
		mut p := pipe()!
		defer {
			p.close()
		}
		assert !fd_is_pending(p.read_fd)
		fd_write(p.write_fd, 'ready')
		assert fd_is_pending(p.read_fd)
		_, n := fd_read(p.read_fd, 5)
		assert n == 5
		assert !fd_is_pending(p.read_fd)
	}
}

$if !windows && !freestanding && !syscalls {
	fn test_fd_is_pending_uses_poll_for_high_descriptors() ! {
		mut p := pipe()!
		mut duplicates := []int{}
		defer {
			for fd in duplicates {
				fd_close(fd)
			}
			p.close()
		}

		mut high_fd := -1
		for _ in 0 .. C.FD_SETSIZE + 1 {
			fd := fd_dup(p.read_fd)
			if fd < 0 {
				break
			}
			duplicates << fd
			if fd >= C.FD_SETSIZE {
				high_fd = fd
				break
			}
		}
		if high_fd < 0 {
			return
		}

		assert !fd_is_pending_poll(high_fd)
		assert !fd_is_pending(high_fd)
		fd_write(p.write_fd, 'ready')
		assert fd_is_pending_poll(high_fd)
		assert fd_is_pending(high_fd)
		_, bytes_read := fd_read(p.read_fd, 5)
		assert bytes_read == 5
		assert !fd_is_pending_poll(high_fd)
		assert !fd_is_pending(high_fd)
		assert fd_close(high_fd) == 0
		duplicates.delete(duplicates.len - 1)
		assert !fd_is_pending(high_fd)
	}
}
