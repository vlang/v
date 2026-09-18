// vtest build: linux && sokol_wayland?
module sapp

fn close_wayland_test_pipe(mut fds [2]i32) {
	for i, fd in fds {
		if fd >= 0 {
			C.close(fd)
			fds[i] = -1
		}
	}
}

fn test_wayland_drop_pipe_has_two_independent_descriptors() {
	mut first := wl_create_drop_pipe()!
	defer {
		close_wayland_test_pipe(mut first)
	}
	mut second := wl_create_drop_pipe()!
	defer {
		close_wayland_test_pipe(mut second)
	}
	assert first[0] >= 0
	assert first[1] >= 0
	assert first[0] != first[1]
	assert second[0] >= 0
	assert second[1] >= 0
	assert second[0] != second[1]
	for fd in first {
		assert fd != second[0]
		assert fd != second[1]
	}
	for i, fds in [first, second] {
		payload := [u8(41 + i)]!
		assert C.write(fds[1], &payload[0], usize(payload.len)) == 1
	}
	for i, fds in [first, second] {
		mut byte := u8(0)
		assert C.read(fds[0], &byte, 1) == 1
		assert byte == u8(41 + i)
	}
}

fn test_wayland_drop_pipe_transfers_uri_list_and_reaches_eof() {
	mut fds := wl_create_drop_pipe()!
	defer {
		close_wayland_test_pipe(mut fds)
	}
	assert fds[0] >= 0 && fds[1] >= 0 && fds[0] != fds[1]
	payload := 'file:///tmp/drop%20path.v\r\nfile:///tmp/caf%C3%A9.v\r\n'
	assert C.write(fds[1], payload.str, usize(payload.len)) == isize(payload.len)
	assert C.close(fds[1]) == 0
	fds[1] = -1
	mut buffer := [128]u8{}
	mut total := 0
	for {
		assert total < buffer.len
		n := C.read(fds[0], unsafe { &buffer[0] + total }, usize(buffer.len - total))
		assert n >= 0
		if n == 0 {
			break
		}
		total += int(n)
	}
	assert buffer[..total].bytestr() == payload
}

fn test_wayland_drop_pipe_empty_writers_reach_eof_repeatedly() {
	// Keeping each pair in a scope exercises cleanup on repeated creation.
	for _ in 0 .. 32 {
		check_empty_wayland_drop_pipe()!
	}
}

fn check_empty_wayland_drop_pipe() ! {
	mut fds := wl_create_drop_pipe()!
	defer {
		close_wayland_test_pipe(mut fds)
	}
	assert fds[0] >= 0 && fds[1] >= 0 && fds[0] != fds[1]
	assert C.close(fds[1]) == 0
	fds[1] = -1
	mut byte := u8(0xa5)
	assert C.read(fds[0], &byte, 1) == 0
	assert byte == 0xa5
}
