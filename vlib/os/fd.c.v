module os

// file descriptor based operations:

// close filedescriptor
pub fn fd_close(fd int) int {
	if fd == -1 {
		return 0
	}
	return C.close(fd)
}

pub fn fd_write(fd int, s string) {
	if fd == -1 {
		return
	}
	mut sp := s.str
	mut remaining := s.len
	for remaining > 0 {
		written := C.write(fd, sp, remaining)
		if written < 0 {
			return
		}
		remaining = remaining - written
		sp = unsafe { voidptr(sp + written) }
	}
}

// read from filedescriptor, block until data
pub fn fd_slurp(fd int) []string {
	mut res := []string{}
	if fd == -1 {
		return res
	}
	for {
		s, b := fd_read(fd, 4096)
		if b <= 0 {
			break
		}
		res << s
	}
	return res
}

// read from filedescriptor, don't block
// return [bytestring,nrbytes]
pub fn fd_read(fd int, maxbytes int) (string, int) {
	if fd == -1 {
		return '', 0
	}
	unsafe {
		mut buf := malloc_noscan(maxbytes + 1)
		nbytes := C.read(fd, buf, maxbytes)
		if nbytes < 0 {
			free(buf)
			return '', nbytes
		}
		buf[nbytes] = 0
		return tos(buf, nbytes), nbytes
	}
}
