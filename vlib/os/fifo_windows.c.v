module os

fn C.CreateNamedPipeW(name &u16, open_mode u32, pipe_mode u32, max_instances u32, out_buf u32, in_buf u32, timeout u32, security voidptr) voidptr
fn C.ConnectNamedPipe(h voidptr, overlapped voidptr) i32
fn C.DisconnectNamedPipe(h voidptr) i32

const fifo_pipe_access_duplex = u32(0x3)
const fifo_error_pipe_connected = u32(535)

// fifo_pipe_name maps a path to a Windows pipe name (directories are invalid
// in pipe names, so only the file name is used).
fn fifo_pipe_name(path string) string {
	name := file_name(path)
	if name == '' {
		return path
	}
	return '\\\\.\\pipe\\' + name
}

// fifo_server_windows creates the named pipe and blocks in ConnectNamedPipe
// until a client connects. A client that connected first (error 535) counts
// as connected.
@[unsafe]
fn fifo_server_windows(path string) !&Fifo {
	name := fifo_pipe_name(path)
	h := C.CreateNamedPipeW(name.to_wide(), fifo_pipe_access_duplex, 0, 1, 65536,
		65536, 0, unsafe { nil })
	if h == voidptr(-1) {
		return error('os: cannot create named pipe `${path}`')
	}
	if C.ConnectNamedPipe(h, unsafe { nil }) == 0 {
		if C.GetLastError() != fifo_error_pipe_connected {
			C.CloseHandle(h)
			return error('os: client failed to connect to `${path}`')
		}
	}
	return &Fifo{
		handle:    h
		is_server: true
	}
}

// fifo_client_windows connects to the named pipe for writing.
@[unsafe]
fn fifo_client_windows(path string) !&Fifo {
	name := fifo_pipe_name(path)
	h := C.CreateFileW(name.to_wide(), 0x40000000, 0, unsafe { nil }, 3, 0x80,
		unsafe { nil })
	if h == voidptr(-1) {
		return error('os: cannot open named pipe `${path}`')
	}
	return &Fifo{
		handle: h
	}
}

// fifo_read_windows reads up to buf.len bytes.
@[unsafe]
fn fifo_read_windows(h voidptr, mut buf []u8) !int {
	if h == unsafe { nil } { return error_file_not_opened() }
	if buf.len == 0 { return Eof{} }
	mut got := u32(0)
	if C.ReadFile(h, buf.data, u32(buf.len), voidptr(&got), unsafe { nil }) == false {
		if C.GetLastError() == 109 { return Eof{} }
		return error('os: named pipe read failed')
	}
	if got == 0 { return Eof{} }
	return int(got)
}

// fifo_write_windows writes buf.
@[unsafe]
fn fifo_write_windows(h voidptr, buf []u8) !int {
	if h == unsafe { nil } { return error_file_not_opened() }
	mut wrote := u32(0)
	if C.WriteFile(h, buf.data, u32(buf.len), voidptr(&wrote), unsafe { nil }) == false {
		return error('os: named pipe write failed')
	}
	return int(wrote)
}

// fifo_close_windows disconnects (servers) and closes the handle.
@[unsafe]
fn fifo_close_windows(h voidptr, is_server bool) {
	if h == unsafe { nil } {
		return
	}
	if is_server {
		C.DisconnectNamedPipe(h)
	}
	C.CloseHandle(h)
}
