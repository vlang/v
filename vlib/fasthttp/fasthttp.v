// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import os
import time
import term
import net

#include <sys/types.h>
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

// Generic POSIX/C definitions
fn C.setsockopt(sockfd int, level int, optname int, optval voidptr, optlen u32) int
fn C.bind(sockfd int, addr voidptr, addrlen u32) int
fn C.listen(sockfd int, backlog int) int
fn C.accept(sockfd int, addr voidptr, addrlen voidptr) int
fn C.fcntl(fd int, cmd int, arg int) int
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

// fn C.snprintf(str &char, size usize, format &char, args ...voidptr) int
fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_mutex_init(mutex &C.pthread_mutex_t, attr voidptr) int
fn C.pthread_mutex_lock(mutex &C.pthread_mutex_t) int
fn C.pthread_mutex_unlock(mutex &C.pthread_mutex_t) int
fn C.pthread_cond_init(cond &C.pthread_cond_t, attr voidptr) int
fn C.pthread_cond_wait(cond &C.pthread_cond_t, mutex &C.pthread_mutex_t) int
fn C.pthread_cond_signal(cond &C.pthread_cond_t) int
fn C.htons(__hostshort u16) u16

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

pub fn (s Slice) str() string {
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
	poll_fd         int // kqueue/epoll fd
	request_handler fn (req HttpRequest) ![]u8 = unsafe { nil }
	worker_data     WorkerData
	threads         [num_threads]C.pthread_t
}

// new_server creates and initializes a new Server instance.
pub fn new_server(port int, handler fn (req HttpRequest) ![]u8) !&Server {
	mut s := &Server{
		port:            port
		request_handler: handler
	}
	return s
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
	// println('worker func')
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
		mut body := s.request_handler(t.req) or { panic('Request handler failed: ${err}') }

		// Copy the body with headers to done.resp
		body_with_headers := body.bytestr()
		resp := C.malloc(buf_size)
		C.snprintf(resp, buf_size, c'%s', body_with_headers.str)
		len := body_with_headers.len

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

		// Wake IO thread (Pipe write is generic POSIX)
		x := u8(`x`)
		C.write(s.worker_data.wake_pipe[1], &x, 1)
		unsafe { C.free(t) }
	}
	return unsafe { nil }
}
