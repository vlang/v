module fasthttp

#include <sys/epoll.h>

// Epoll constants
const epoll_ctl_add = 1
const epoll_ctl_del = 2
const epoll_ctl_mod = 3
const epoll_in      = 1
const epoll_out     = 4
const epoll_err     = 8
const epoll_hup     = 16
const epoll_rdhup   = 8192

union C.epoll_data {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

struct C.epoll_event {
mut:
	events u32
	data   C.epoll_data
}

fn C.epoll_create1(flags int) int
fn C.epoll_ctl(epfd int, op int, fd int, event &C.epoll_event) int
fn C.epoll_wait(epfd int, events &C.epoll_event, maxevents int, timeout int) int

// Helper to wrap epoll_ctl for cleaner code
fn control_epoll(epfd int, op int, fd int, events u32, data voidptr) {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.ptr = data
	C.epoll_ctl(epfd, op, fd, &ev)
}

// process_dones handles connections that have been processed by a worker thread.
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
			// Not all data sent, add WRITE event
			// Note: The connection was removed from epoll before sending to worker, so we ADD here.
			control_epoll(s.poll_fd, epoll_ctl_add, c.fd, u32(epoll_out | epoll_rdhup), c)
		} else {
			// Response sent, re-enable reading for keep-alive
			C.free(c.write_buf)
			c.write_buf = unsafe { nil }
			
			// Note: The connection was removed from epoll before sending to worker, so we ADD here.
			control_epoll(s.poll_fd, epoll_ctl_add, c.fd, u32(epoll_in | epoll_rdhup), c)
			c.read_len = 0
		}
		unsafe { C.free(d) }
	}
}

// run starts the server and enters the main event loop (Epoll version).
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
	addr.sin_family = u16(C.AF_INET)
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

	// Create epoll instance
	s.poll_fd = C.epoll_create1(0)
	if s.poll_fd < 0 {
		C.perror(c'epoll_create1')
		return error('epoll creation failed')
	}

	// Add listener socket to epoll
	// We pass the fd as the pointer value to identify it later.
	control_epoll(s.poll_fd, epoll_ctl_add, s.socket_fd, u32(epoll_in), voidptr(isize(s.socket_fd)))

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

	// Add wake pipe to epoll
	control_epoll(s.poll_fd, epoll_ctl_add, s.worker_data.wake_pipe[0], u32(epoll_in), voidptr(isize(s.worker_data.wake_pipe[0])))

	// Create worker threads
	for i := 0; i < num_threads; i++ {
		C.pthread_create(&s.threads[i], unsafe { nil }, worker_func, s)
	}

	println('Server listening on port ${s.port}')

	// Event loop
	events := [64]C.epoll_event{}
	for {
		nev := C.epoll_wait(s.poll_fd, &events[0], 64, -1)
		if nev < 0 {
			C.perror(c'epoll_wait')
			break
		}

		for i := 0; i < nev; i++ {
			event := events[i]
			ptr_val := isize(event.data.ptr)

			// 1. Check for Listener Socket
			if ptr_val == s.socket_fd {
				client_fd := C.accept(s.socket_fd, unsafe { nil }, unsafe { nil })
				if client_fd < 0 {
					continue
				}
				mut new_c := unsafe { &Conn(C.malloc(sizeof(Conn))) }
				C.memset(new_c, 0, sizeof(Conn))
				new_c.fd = client_fd
				C.fcntl(new_c.fd, C.F_SETFL, C.O_NONBLOCK)
				
				control_epoll(s.poll_fd, epoll_ctl_add, new_c.fd, u32(epoll_in | epoll_rdhup), new_c)
				continue
			} 

			// 2. Check for Wake Pipe (Worker finished a task)
			if ptr_val == s.worker_data.wake_pipe[0] {
				buf := [1024]u8{}
				for C.read(s.worker_data.wake_pipe[0], &buf[0], sizeof(buf)) > 0 {}
				s.process_dones()
				continue
			}

			// 3. Client Connection
			mut c := unsafe { &Conn(event.data.ptr) }

			// Handle Errors or HUP
			if (event.events & u32(epoll_err | epoll_hup | epoll_rdhup)) != 0 {
				s.close_conn(c)
				continue
			}

			// Handle Read
			if (event.events & u32(epoll_in)) != 0 {
				n := C.read(c.fd, &c.read_buf[c.read_len], buf_size - c.read_len)
				if n <= 0 {
					if n < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
						s.close_conn(c)
					} else if n == 0 {
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
				// Remove from epoll so we don't trigger while worker is busy
				control_epoll(s.poll_fd, epoll_ctl_del, c.fd, 0, unsafe { nil })

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
			
			} else if (event.events & u32(epoll_out)) != 0 { // Handle Write
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
					// Done writing, modify epoll to stop listening for OUT and start listening for IN
					control_epoll(s.poll_fd, epoll_ctl_mod, c.fd, u32(epoll_in | epoll_rdhup), c)
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
