module main

import io.multiplexing.epoll
import time

const max_iterations = 1_000_000

fn main() {
	epoll_fd := epoll.create()
	if epoll_fd < 0 {
		eprintln('Failed to create epoll instance')
		return
	}
	mut fds := [0, 0]!
	res := C.pipe(&fds[0])
	if res != 0 {
		eprintln('Failed to create pipe')
		epoll_fd.close()
		return
	}
	read_fd := fds[0]
	write_fd := fds[1]
	// Add read_fd to epoll for read events (EPOLLIN = 1)
	ret := epoll_fd.add_fd(read_fd, 1)
	if ret != 0 {
		eprintln('Failed to add fd to epoll')
		C.close(write_fd)
		epoll_fd.close()
		return
	}
	// Benchmark loop

	mut total_time := i64(0)
	msg := 'x'
	mut events := epoll.EpollEvent{}
	for _ in 0 .. max_iterations {
		// Write to pipe to trigger event
		C.write(write_fd, msg.str, msg.len)
		start := time.ticks()
		n := epoll_fd.wait_for_events(mut events, 1, 1000)
		elapsed := time.ticks() - start
		total_time += elapsed
		if n != 1 {
			eprintln('epoll_wait did not return 1')
			break
		}
		// Read the data back
		mut buf := []u8{len: msg.len}
		C.read(read_fd, unsafe { &buf[0] }, msg.len)
	}
	println('Epoll wait benchmark:')
	println('Iterations: ${max_iterations}')
	println('Total time (ms): ${total_time}')
	println('Average time per wait (us): ${(total_time * 1000) / max_iterations}')
	// Cleanup
	epoll_fd.remove_fd(read_fd)
	C.close(write_fd)
	epoll_fd.close()
}
