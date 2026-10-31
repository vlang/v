module os

fn C.mkfifo(path &char, mode u32) i32
fn C.fstat(fd i32, buf &C.stat) i32

// fifo_open_nix opens an existing FIFO without creating or truncating regular files.
fn fifo_open_nix(path string, flags int, mode &char) !File {
	fd := C.open(&char(path.str), flags)
	if fd == -1 { return error_posix() }
	mut st := C.stat{}
	if C.fstat(fd, &st) != 0 || u32(st.st_mode) & u32(C.S_IFMT) != u32(C.S_IFIFO) {
		C.close(fd)
		return error('os: `${path}` is not a fifo')
	}
	cfile := C.fdopen(fd, mode)
	if isnil(cfile) {
		C.close(fd)
		return error('os: cannot open fifo `${path}`')
	}
	return File{ cfile: cfile, fd: fd, is_opened: true }
}

// fifo_server_nix creates the FIFO and blocks until a writer connects.
fn fifo_server_nix(path string, mode u32) !&Fifo {
	if C.mkfifo(&char(path.str), mode) != 0 { return error_posix() }
	f := fifo_open_nix(path, C.O_RDONLY, c'rb') or {
		rm(path) or {}
		return err
	}
	return &Fifo{ file: f, is_server: true }
}

// fifo_client_nix opens an existing FIFO and blocks until a reader connects.
fn fifo_client_nix(path string) !&Fifo {
	f := fifo_open_nix(path, C.O_WRONLY, c'wb')!
	return &Fifo{ file: f }
}

// fifo_read_nix reads currently available bytes without stdio buffering.
fn fifo_read_nix(f File, mut buf []u8) !int {
	if !f.is_opened { return error_file_not_opened() }
	if buf.len == 0 { return Eof{} }
	for {
		n := C.read(f.fd, buf.data, uint(buf.len))
		if n < 0 {
			if C.errno == C.EINTR { continue }
			return error_posix()
		}
		if n == 0 { return Eof{} }
		return int(n)
	}
	return error('os: unreachable fifo read')
}

// fifo_write_nix writes directly to the pipe so readers can receive before close.
fn fifo_write_nix(f File, buf []u8) !int {
	if !f.is_opened { return error_file_not_opened() }
	for {
		n := C.write(f.fd, buf.data, uint(buf.len))
		if n < 0 {
			if C.errno == C.EINTR { continue }
			return error_posix()
		}
		return int(n)
	}
	return error('os: unreachable fifo write')
}
