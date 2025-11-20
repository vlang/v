// fasthttp_darwin.v
module fasthttp

#include <sys/event.h>

struct C.kevent {
	ident  u64
	filter i16
	flags  u16
	fflags u32
	data   isize
	udata  voidptr
}

fn C.kqueue() int
fn C.kevent(kq int, changelist &C.kevent, nchanges int, eventlist &C.kevent, nevents int, timeout &C.timespec) int

// Helper to set fields of a kevent struct
fn ev_set(mut ev C.kevent, ident u64, filter i16, flags u16, fflags u32, data isize, udata voidptr) {
	ev.ident = ident
	ev.filter = filter
	ev.flags = flags
	ev.fflags = fflags
	ev.data = data
	ev.udata = udata
}

// process_dones handles connections that have been processed by a worker thread.
// It manipulates the Kqueue state (EVFILT_WRITE/READ)
fn (mut s Server) process_dones() {
	C.pthread_mutex_lock(&s.worker_data.done_mutex)
	mut local_head := s.worker_data.done_head
	s.worker_data.done_head = unsafe { nil }
	s.worker_data.done_tail = unsafe { nil }
	C.pthread_mutex_unlock(&s.worker_data.done_mutex)

	for local_head != unsafe { nil } {
		d := local_head
		local_head = d.next
		mut c := d.c
		c.write_buf = d.resp
		c.write_len = d.len
		c.write_pos = 0

		// Try to write immediately
		write_ptr := unsafe { &u8(c.write_buf) + c.write_pos }
		written := C.write(c.fd, write_ptr, c.write_len - c.write_pos)
		if written > 0 {
			c.write_pos += int(written)
		} else if written < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
			s.close_conn(c)
			unsafe { C.free(d) }
			continue
		}

		if c.write_pos < c.write_len {
			// Add write event if not all data was sent
			mut ev := C.kevent{}
			ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_ADD | C.EV_EOF),
				u32(0), isize(0), c)
			C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })
		} else {
			// Response sent, re-enable reading for keep-alive
			C.free(c.write_buf)
			c.write_buf = unsafe { nil }
			mut ev := C.kevent{}
			ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_EOF), u32(0),
				isize(0), c)
			C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })
			c.read_len = 0
		}
		unsafe { C.free(d) }
	}
}

// run starts the server and enters the main event loop (Kqueue version).
pub fn (mut s Server) run() ! {
	// Create server socket
	s.socket_fd = C.socket(.ip, .tcp, 0)
	if s.socket_fd < 0 {
		C.perror(c'socket')
		return error('socket creation failed')
	}

	opt := 1
	C.setsockopt(s.socket_fd, C.SOL_SOCKET, C.SO_REUSEADDR, &opt, sizeof(int))

	mut addr := C.sockaddr_in{}
	C.memset(&addr, 0, sizeof(addr))
	addr.sin_family = C.AF_INET
	addr.sin_port = u16(C.htons(u16(s.port)))

	if C.bind(s.socket_fd, voidptr(&addr), sizeof(addr)) < 0 {
		C.perror(c'bind')
		return error('socket bind failed')
	}
	if C.listen(s.socket_fd, backlog) < 0 {
		C.perror(c'listen')
		return error('socket listen failed')
	}
	C.fcntl(s.socket_fd, C.F_SETFL, C.O_NONBLOCK)

	// Create kqueue
	s.poll_fd = C.kqueue()
	if s.poll_fd < 0 {
		C.perror(c'kqueue')
		return error('kqueue creation failed')
	}

	mut ev := C.kevent{}
	ev_set(mut &ev, u64(s.socket_fd), i16(C.EVFILT_READ), u16(C.EV_ADD), u32(0), isize(0),
		unsafe { nil })
	C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })

	// Initialize worker data
	C.pthread_mutex_init(&s.worker_data.task_mutex, unsafe { nil })
	C.pthread_cond_init(&s.worker_data.task_cond, unsafe { nil })
	C.pthread_mutex_init(&s.worker_data.done_mutex, unsafe { nil })

	// Create wake pipe
	if C.pipe(&s.worker_data.wake_pipe[0]) < 0 {
		C.perror(c'pipe')
		return error('pipe creation failed')
	}
	C.fcntl(s.worker_data.wake_pipe[0], C.F_SETFL, C.O_NONBLOCK)
	C.fcntl(s.worker_data.wake_pipe[1], C.F_SETFL, C.O_NONBLOCK)

	// Add wake pipe to kqueue
	ev_set(mut &ev, u64(s.worker_data.wake_pipe[0]), i16(C.EVFILT_READ), u16(C.EV_ADD),
		u32(0), isize(0), unsafe { nil })
	C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })

	// Create worker threads
	for i := 0; i < num_threads; i++ {
		C.pthread_create(&s.threads[i], unsafe { nil }, worker_func, s)
	}

	println('Server listening on port ${s.port}')

	// Event loop
	events := [64]C.kevent{}
	for {
		nev := C.kevent(s.poll_fd, unsafe { nil }, 0, &events[0], 64, unsafe { nil })
		if nev < 0 {
			C.perror(c'kevent')
			break
		}

		for i := 0; i < nev; i++ {
			event := events[i]
			mut c := unsafe { &Conn(event.udata) }

			if event.flags & u16(C.EV_ERROR) != 0 {
				if c != unsafe { nil } {
					s.close_conn(c)
				}
				continue
			}

			if event.ident == u64(s.socket_fd) { // New connection
				client_fd := C.accept(s.socket_fd, unsafe { nil }, unsafe { nil })
				if client_fd < 0 {
					continue
				}
				mut new_c := unsafe { &Conn(C.malloc(sizeof(Conn))) }
				C.memset(new_c, 0, sizeof(Conn))
				new_c.fd = client_fd
				C.fcntl(new_c.fd, C.F_SETFL, C.O_NONBLOCK)
				ev_set(mut &ev, u64(new_c.fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_EOF),
					u32(0), isize(0), new_c)
				C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })
			} else if event.ident == u64(s.worker_data.wake_pipe[0]) { // Worker is done
				buf := [1024]u8{}
				for C.read(s.worker_data.wake_pipe[0], &buf[0], sizeof(buf)) > 0 {}
				s.process_dones()
			} else if event.filter == i16(C.EVFILT_READ) { // Data from client
				if event.flags & u16(C.EV_EOF) != 0 {
					s.close_conn(c)
					continue
				}
				n := C.read(c.fd, &c.read_buf[c.read_len], buf_size - c.read_len)
				if n <= 0 {
					if n < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
						s.close_conn(c)
					}
					continue
				}
				c.read_len += int(n)

				header_end := C.memmem(&c.read_buf[0], c.read_len, c'\r\n\r\n', 4)
				if header_end == unsafe { nil } {
					if c.read_len >= buf_size {
						s.close_conn(c)
					}
					continue
				}

				if C.memcmp(&c.read_buf[0], c'GET ', 4) != 0 {
					s.close_conn(c)
					continue
				}
				path_start := &c.read_buf[4]
				path_end := C.strchr(path_start, ` `)
				if path_end == unsafe { nil } {
					s.close_conn(c)
					continue
				}
				path_len := unsafe { path_end - path_start }

				req := HttpRequest{
					buffer:         c.read_buf[..c.read_len]
					method:         Slice{
						buf: &c.read_buf[0]
						len: 3
					}
					path:           Slice{
						buf: path_start
						len: path_len
					}
					client_conn_fd: c.fd
				}

				c.read_len = 0

				// Offload to worker thread
				// DELETE Read event so we don't trigger while worker is busy
				ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_READ), u16(C.EV_DELETE), u32(0),
					isize(0), c)
				C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })

				mut t := unsafe { &Task(C.malloc(sizeof(Task))) }
				t.c = c
				t.req = req
				t.next = unsafe { nil }

				C.pthread_mutex_lock(&s.worker_data.task_mutex)
				if s.worker_data.task_tail != unsafe { nil } {
					s.worker_data.task_tail.next = t
				} else {
					s.worker_data.task_head = t
				}
				s.worker_data.task_tail = t
				C.pthread_cond_signal(&s.worker_data.task_cond)
				C.pthread_mutex_unlock(&s.worker_data.task_mutex)
			} else if event.filter == i16(C.EVFILT_WRITE) { // Ready to write more data
				if event.flags & u16(C.EV_EOF) != 0 {
					s.close_conn(c)
					continue
				}
				write_ptr := unsafe { &u8(c.write_buf) + c.write_pos }
				written := C.write(c.fd, write_ptr, c.write_len - c.write_pos)
				if written > 0 {
					c.write_pos += int(written)
				} else if written < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
					s.close_conn(c)
					continue
				}

				if c.write_pos >= c.write_len {
					C.free(c.write_buf)
					c.write_buf = unsafe { nil }
					// Done writing, delete write filter
					ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_DELETE),
						u32(0), isize(0), c)
					C.kevent(s.poll_fd, &ev, 1, unsafe { nil }, 0, unsafe { nil })

					c.read_len = 0
				}
			}
		}
	}

	C.close(s.socket_fd)
	C.close(s.poll_fd)
	C.close(s.worker_data.wake_pipe[0])
	C.close(s.worker_data.wake_pipe[1])
}
