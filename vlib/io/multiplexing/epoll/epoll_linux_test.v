import io.multiplexing.epoll

fn test_create_epoll_fd() {
	fd := epoll.create()
	assert fd >= 0
	if fd >= 0 {
		// Clean up
		epoll.close(fd)
	}
}

fn test_epoll_wait_for_read_event() {
	epoll_fd := epoll.create()
	assert epoll_fd >= 0
	if epoll_fd < 0 {
		return
	}
	// Create a pipe
	mut fds := [0, 0]!
	res := C.pipe(&fds[0])
	assert res == 0
	read_fd := fds[0]
	write_fd := fds[1]
	// Add read_fd to epoll for read events (EPOLLIN = 1)
	ret := epoll.add_fd_to_epoll(epoll_fd, read_fd, 1)
	assert ret == 0
	// Write data to the pipe to trigger the event
	msg := 'vlang!'
	written := C.write(write_fd, msg.str, msg.len)
	assert written == msg.len
	// Prepare event struct
	mut events := C.epoll_event{}
	// Wait for the event (timeout 1000ms)
	n := C.epoll_wait(epoll_fd, &events, 1, 1000)
	assert n == 1
	assert (events.events & 1) != 0 // EPOLLIN
	assert unsafe { events.data.fd } == read_fd
	// Read the data back
	mut buf := []u8{len: msg.len}
	nread := C.read(read_fd, unsafe { &buf[0] }, msg.len)
	assert nread == msg.len
	assert buf.bytestr() == msg
	// Cleanup
	epoll.remove_fd_from_epoll(epoll_fd, read_fd)
	epoll.close(write_fd)
	epoll.close(epoll_fd)
}

fn test_add_and_remove_fd_to_epoll() {
	epoll_fd := epoll.create()
	assert epoll_fd >= 0
	if epoll_fd < 0 {
		return
	}
	// Use a pipe to get a valid fd
	mut fds := [0, 0]!
	res := C.pipe(&fds[0])
	assert res == 0
	read_fd := fds[0]
	write_fd := fds[1]
	// Add read_fd to epoll
	ret := epoll.add_fd_to_epoll(epoll_fd, read_fd, 1) // 1 = EPOLLIN
	assert ret == 0
	// Remove read_fd from epoll
	epoll.remove_fd_from_epoll(epoll_fd, read_fd)
	// Clean up
	epoll.close(write_fd)
	epoll.close(epoll_fd)
}
