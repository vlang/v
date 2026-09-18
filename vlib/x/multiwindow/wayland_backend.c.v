	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
	fn C.strcmp(a &char, b &char) int
	fn C.close(fd int) int
	fn C.pipe(fds &i32) i32
	fn C.read(fd int, buf voidptr, count usize) isize
	fn C.write(fd int, buf voidptr, count usize) isize
	fn C.mmap(addr voidptr, length usize, prot int, flags int, fd int, offset i64) voidptr
	fn C.munmap(addr voidptr, length usize) int
