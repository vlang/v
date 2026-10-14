module os

// Fifo is a named-pipe endpoint for IPC between related or unrelated
// processes. The server creates the endpoint and blocks waiting for a client;
// the client connects to it. Afterwards bytes flow in one
// direction: client writes, server reads (open a second pair for duplex).
// It is the cross-platform form of POSIX fifos and Windows named pipes.
@[heap]
pub struct Fifo {
mut:
	file      File
	handle    voidptr
	is_server bool
}

// fifo_server creates the endpoint at path and blocks until a client
// connects, then returns it for reading. On POSIX path is a filesystem node
// (removed by the caller when done); on Windows it is used as the pipe name.
// mode is the POSIX permission bits (ignored on Windows).
pub fn fifo_server(path string, mode u32) !&Fifo {
	$if windows {
		return fifo_server_windows(path)
	} $else {
		return fifo_server_nix(path, mode)
	}
}

// fifo_client connects to the endpoint at path for writing (blocks until a
// server is waiting).
pub fn fifo_client(path string) !&Fifo {
	$if windows {
		return fifo_client_windows(path)
	} $else {
		return fifo_client_nix(path)
	}
}

// read reads up to buf.len bytes. It mirrors File.read.
pub fn (f &Fifo) read(mut buf []u8) !int {
	$if windows {
		return fifo_read_windows(f.handle, mut buf)
	} $else {
		return fifo_read_nix(f.file, mut buf)
	}
}

// write writes buf. It mirrors File.write.
pub fn (mut f Fifo) write(buf []u8) !int {
	$if windows {
		return fifo_write_windows(f.handle, buf)
	} $else {
		return fifo_write_nix(f.file, buf)
	}
}

// close releases the endpoint.
pub fn (mut f Fifo) close() {
	$if windows {
		fifo_close_windows(f.handle, f.is_server)
		f.handle = nil
	} $else {
		f.file.close()
	}
}
