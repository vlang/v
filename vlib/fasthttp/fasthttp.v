// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import os
import time
import term
import net

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

fn C.setsockopt(sockfd int, level int, optname int, optval voidptr, optlen u32) int
fn C.bind(sockfd int, addr voidptr, addrlen u32) int
fn C.listen(sockfd int, backlog int) int
fn C.accept(sockfd int, addr voidptr, addrlen voidptr) int
fn C.fcntl(fd int, cmd int, arg int) int
fn C.kqueue() int
fn C.kevent(kq int, changelist &C.kevent, nchanges int, eventlist &C.kevent, nevents int, timeout &C.timespec) int
fn C.pipe(pipefd &int) int
fn C.close(fd int) int
fn C.read(fd int, buf voidptr, count int) int
fn C.write(fd int, buf voidptr, count int) int
fn C.malloc(size int) &u8
fn C.free(ptr voidptr)
fn C.memset(dest voidptr, ch int, count int) voidptr
fn C.memcmp(s1 voidptr, s2 voidptr, n int) int
fn C.memmem(haystack voidptr, haystacklen int, needle voidptr, needlelen int) voidptr
fn C.strchr(s &u8, c int) &u8
fn C.perror(s &char)
fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_mutex_init(mutex &C.pthread_mutex_t, attr voidptr) int
fn C.pthread_mutex_lock(mutex &C.pthread_mutex_t) int
fn C.pthread_mutex_unlock(mutex &C.pthread_mutex_t) int
fn C.pthread_cond_init(cond &C.pthread_cond_t, attr voidptr) int
fn C.pthread_cond_wait(cond &C.pthread_cond_t, mutex &C.pthread_mutex_t) int
fn C.pthread_cond_signal(cond &C.pthread_cond_t) int
fn C.htons(__hostshort u16) u16

struct C.kevent {
	ident  u64
	filter i16
	flags  u16
	fflags u32
	data   isize
	udata  voidptr
}

struct C.sockaddr_in {
mut:
	sin_len    u8
	sin_family u8
	sin_port   u16
	sin_addr   u32
	sin_zero   [8]char
}

const backlog = 128
const buf_size = 8_000
const num_threads = 8

// Slice represents a part of a larger buffer, without owning the memory.
// It's useful for representing parts of the request buffer like the method and path.
pub struct Slice {
pub mut:
	buf &u8 = unsafe { nil }
	len int
}

// str returns the V string representation of the slice.
pub fn (s Slice) str() string {
	// return unsafe { string(s.buf, s.len) }
	return unsafe { s.buf.vstring_with_len(s.len) }
}

// HttpRequest represents a parsed HTTP request. The slices point to memory
// within the connection's buffer and are only valid for the duration of the request.
pub struct HttpRequest {
pub mut:
	buffer         []u8 // A V slice of the read buffer for convenience
	method         Slice
	path           Slice
	version        Slice
	client_conn_fd int
}

// Internal struct to hold connection-specific data
struct Conn {
mut:
	fd        int
	read_buf  [buf_size]u8
	read_len  int
	write_buf voidptr
	write_len int
	write_pos int
}

// Task for the worker thread pool
struct Task {
mut:
	c    &Conn = unsafe { nil }
	req  HttpRequest // Pass the parsed request to the worker
	next &Task = unsafe { nil }
	// req_buffer []u8 // The worker will own this copied buffer
}

// Completed task data
struct Done {
mut:
	c    &Conn
	resp voidptr
	len  int
	next &Done
}

// Shared data for worker threads
struct WorkerData {
mut:
	task_mutex C.pthread_mutex_t
	task_cond  C.pthread_cond_t
	task_head  &Task = unsafe { nil }
	task_tail  &Task = unsafe { nil }
	done_mutex C.pthread_mutex_t
	done_head  &Done = unsafe { nil }
	done_tail  &Done = unsafe { nil }
	quit       bool
	wake_pipe  [2]int
}

// Server holds the entire state of the web server instance.
pub struct Server {
pub mut:
	port            int
	socket_fd       int
	kq              int
	request_handler fn (req HttpRequest) ![]u8 = unsafe { nil }
	worker_data     WorkerData
	threads         [num_threads]C.pthread_t
}

// new_server creates and initializes a new Server instance.
pub fn new_server(port int, handler fn (req HttpRequest) ![]u8) !&Server {
	mut s := &Server{
		port:            port
		request_handler: handler
		// worker_data:
	}
	return s
}

// Helper to set fields of a kevent struct, replacing the C macro EV_SET
fn ev_set(mut ev C.kevent, ident u64, filter i16, flags u16, fflags u32, data isize, udata voidptr) {
	ev.ident = ident
	ev.filter = filter
	ev.flags = flags
	ev.fflags = fflags
	ev.data = data
	ev.udata = udata
}

fn (mut s Server) close_conn(c &Conn) {
	if c.write_buf != unsafe { nil } {
		C.free(c.write_buf)
	}
	C.close(c.fd)
	unsafe { C.free(c) }
}

// worker_func is the function executed by each worker thread.
// It processes tasks from the queue, calls the request handler,
// and puts the result in the 'done' queue.
fn worker_func(arg voidptr) voidptr {
	mut s := unsafe { &Server(arg) }
	for {
		C.pthread_mutex_lock(&s.worker_data.task_mutex)
		for s.worker_data.task_head == unsafe { nil } && !s.worker_data.quit {
			C.pthread_cond_wait(&s.worker_data.task_cond, &s.worker_data.task_mutex)
		}
		if s.worker_data.quit && s.worker_data.task_head == unsafe { nil } {
			C.pthread_mutex_unlock(&s.worker_data.task_mutex)
			break
		}
		mut t := s.worker_data.task_head
		s.worker_data.task_head = t.next
		if s.worker_data.task_head == unsafe { nil } {
			s.worker_data.task_tail = unsafe { nil }
		}
		C.pthread_mutex_unlock(&s.worker_data.task_mutex)
		// Call the user-provided request handler to get the response body
		mut body := s.request_handler(t.req) or {
			// On handler error, create a 500 response
			// eprintln('Request handler failed: ${err}')
			panic('Request handler failed: ${err}')
			//[]u8('<h1>Internal Server Error</h1>')
		}
		// println('body.len=${body.len} body=${body.bytestr()}')
		// println('=============')
		// println('ALL')
		body_with_headers := body.bytestr()
		// println(body_with_headers)
		// println('============')
		// println('body')
		// body = (body.bytestr().all_after('Server: veb').trim_space()).bytes()
		// println(body)
		// println('============')
		// Copy the body with headers to done.resp
		resp := C.malloc(buf_size)
		C.snprintf(resp, buf_size, c'%s', body_with_headers.str)
		len := body_with_headers.len
		// println('GGGG len=${len} body.len=${body.len} full body len = ${body_with_headers.len}')
		// Enqueue done
		mut d := unsafe { &Done(C.malloc(sizeof(Done))) }
		d.c = t.c
		d.resp = resp
		d.len = int(len)
		d.next = unsafe { nil }
		C.pthread_mutex_lock(&s.worker_data.done_mutex)
		if s.worker_data.done_tail != unsafe { nil } {
			s.worker_data.done_tail.next = d
		} else {
			s.worker_data.done_head = d
		}
		s.worker_data.done_tail = d
		C.pthread_mutex_unlock(&s.worker_data.done_mutex)
		// Wake IO thread
		x := u8(`x`)
		C.write(s.worker_data.wake_pipe[1], &x, 1)
		unsafe { C.free(t) }
	}
	return unsafe { nil }
}

// process_dones handles connections that have been processed by a worker thread.
fn (mut s Server) process_dones(kq int) {
	// println('process_dones')
	C.pthread_mutex_lock(&s.worker_data.done_mutex)
	mut local_head := s.worker_data.done_head
	s.worker_data.done_head = unsafe { nil }
	s.worker_data.done_tail = unsafe { nil }
	C.pthread_mutex_unlock(&s.worker_data.done_mutex)

	for local_head != unsafe { nil } {
		// println('FOR')
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
			// println('if1')
			// Add write event if not all data was sent
			mut ev := C.kevent{}
			ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_ADD | C.EV_EOF),
				u32(0), isize(0), c)
			C.kevent(kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
		} else {
			// println('if2')
			// Response sent, re-enable reading for keep-alive
			C.free(c.write_buf)
			c.write_buf = unsafe { nil }
			mut ev := C.kevent{}
			ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_EOF), u32(0),
				isize(0), c)
			C.kevent(kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
			c.read_len = 0
		}
		unsafe { C.free(d) }
	}
}

// const C.AF_INET u8 // run starts the server and enters the main event loop.

pub fn (mut s Server) run() ! {
	// Create server socket
	// s.socket_fd = C.socket(C.AF_INET, C.SOCK_STREAM, 0)
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
	// addr.sin_addr = u32(0) // C.htons(C.INADDR_ANY))
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
	s.kq = C.kqueue()
	if s.kq < 0 {
		C.perror(c'kqueue')
		return error('kqueue creation failed')
	}

	mut ev := C.kevent{}
	ev_set(mut &ev, u64(s.socket_fd), i16(C.EVFILT_READ), u16(C.EV_ADD), u32(0), isize(0),
		unsafe { nil })
	C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })

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
	ev_set(mut &ev, u64(s.worker_data.wake_pipe[0]), i16(C.EVFILT_READ), u16(C.EV_ADD),
		u32(0), isize(0), unsafe { nil })
	C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })

	// Create worker threads
	for i := 0; i < num_threads; i++ {
		C.pthread_create(&s.threads[i], unsafe { nil }, worker_func, s)
	}

	println('Server listening on port ${s.port}')

	// Event loop
	events := [64]C.kevent{}
	for {
		nev := C.kevent(s.kq, unsafe { nil }, 0, &events[0], 64, unsafe { nil })
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
				C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
			} else if event.ident == u64(s.worker_data.wake_pipe[0]) { // Worker is done
				buf := [1024]u8{}
				for C.read(s.worker_data.wake_pipe[0], &buf[0], sizeof(buf)) > 0 {}
				s.process_dones(s.kq)
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
						s.close_conn(c) // Headers too large
					}
					continue
				}

				// Simple parse
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
				// path_len := unsafe { path_end - &char(path_start) }
				path_len := unsafe { path_end - path_start }

				// Create HttpRequest for the handler
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

				// Consume request from buffer, assume no body
				c.read_len = 0

				// The conditional check for '/sleep' has been removed.
				// All requests are now offloaded to the worker threads.

				// Offload to worker thread
				ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_READ), u16(C.EV_DELETE), u32(0),
					isize(0), c)
				C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })

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
					ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_DELETE),
						u32(0), isize(0), c)
					C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })

					/*
					// *** THIS IS THE FIX ***
					// Re-enable the READ filter to listen for the next request (e.g., for the CSS file)
					ev_set(mut &ev, u64(c.fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_EOF),
						u32(0), isize(0), c)
					C.kevent(s.kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
					// ***********************
					*/

					c.read_len = 0
				}
			}
		}
	}

	// Cleanup (not reached in this simple example)
	C.close(s.socket_fd)
	C.close(s.kq)
	C.close(s.worker_data.wake_pipe[0])
	C.close(s.worker_data.wake_pipe[1])
}
