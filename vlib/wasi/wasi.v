module wasi

// WASI Preview 1 helpers for V programs compiled to WebAssembly (wasm32-wasi).

pub struct IOVec {
pub:
	buf &u8
	len uint
}

pub fn fd_write(fd int, iovs []IOVec, nwritten &uint) int {
	$if wasm32_wasi {
		if iovs.len == 0 {
			return 0
		}
		mut errno := 0
		unsafe {
			errno = int(WASM.fd_write(fd, &iovs[0], uint(iovs.len), nwritten))
		}
		return errno
	} $else {
		_ = fd
		_ = iovs
		_ = nwritten
		return 0
	}
}

pub fn fd_read(fd int, iovs []IOVec, nread &uint) int {
	$if wasm32_wasi {
		if iovs.len == 0 {
			return 0
		}
		mut errno := 0
		unsafe {
			errno = int(WASM.fd_read(fd, &iovs[0], uint(iovs.len), nread))
		}
		return errno
	} $else {
		_ = fd
		_ = iovs
		_ = nread
		return 0
	}
}

pub fn fd_sync(fd int) int {
	$if wasm32_wasi {
		return int(WASM.fd_sync(fd))
	} $else {
		_ = fd
		return 0
	}
}

pub fn random_get(buf []u8) int {
	$if wasm32_wasi {
		if buf.len == 0 {
			return 0
		}
		mut errno := 0
		unsafe {
			errno = int(WASM.random_get(&buf[0], uint(buf.len)))
		}
		return errno
	} $else {
		_ = buf
		return 0
	}
}

pub fn args_sizes_get() (int, int) {
	$if wasm32_wasi {
		mut argc := u32(0)
		mut argc_buf_size := u32(0)
		unsafe {
			WASM.args_sizes_get(&argc, &argc_buf_size)
		}
		return int(argc), int(argc_buf_size)
	} $else {
		return 0, 0
	}
}

pub fn proc_exit(code int) {
	$if wasm32_wasi {
		WASM.proc_exit(code)
	} $else {
		_ = code
	}
}