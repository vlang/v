module websocket

import encoding.utf8
import math
import net
import sync.stdatomic
import time

#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/socket.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

@[typedef]
union C.epoll_data_t {
mut:
	u64 u64
}

struct C.epoll_event {
mut:
	events u32
	data   C.epoll_data_t
}

fn C.epoll_create1(int) int
fn C.epoll_ctl(int, int, int, &C.epoll_event) int
fn C.epoll_wait(int, &C.epoll_event, int, int) int
fn C.eventfd(u32, int) int
fn C.pthread_self() usize

// ReactorOptions configures an optional Linux plaintext server transport.
// Callbacks run serially on the owning worker, must not block, and borrow each
// message payload only until on_message returns. Clone retained payloads.
@[params]
pub struct ReactorOptions {
pub:
	max_message_bytes    int                                          = 16384
	max_pending_messages int                                          = 64
	max_pending_bytes    int                                          = 2 * 1024 * 1024
	max_connections      int                                          = 10000
	max_commands         int                                          = 8192
	max_command_bytes    int                                          = 16 * 1024 * 1024
	frames_per_turn      int                                          = 256
	read_timeout         time.Duration                                = 5 * time.second
	write_timeout        time.Duration                                = 5 * time.second
	close_timeout        time.Duration                                = time.second
	on_open              fn (mut ReactorClient, voidptr)              = reactor_no_open
	on_message           fn (mut ReactorClient, &Message, voidptr)    = reactor_no_message
	on_close             fn (mut ReactorClient, int, string, voidptr) = reactor_no_close
	user                 voidptr
}

fn reactor_no_open(mut _c ReactorClient, _ref voidptr) {}

fn reactor_no_message(mut _c ReactorClient, _m &Message, _ref voidptr) {}

fn reactor_no_close(mut _c ReactorClient, _code int, _reason string, _ref voidptr) {}

// ReactorClient is a thread-safe handle. Successful methods accept commands,
// not delivery receipts. Every successful attach has exactly one on_close,
// including an asynchronous failure before on_open. Failed attach retains the
// caller's socket ownership and produces no callbacks.
@[heap]
pub struct ReactorClient {
pub:
	id string
mut:
	owner      &Reactor = unsafe { nil }
	key        u64
	state      u64 // 0 pending, 1 open, 2 closing, 3 closed
	overloaded u64
}

enum ReactorCommandKind {
	attach
	send
	close
	timeout
}

struct ReactorCommand {
	kind       ReactorCommandKind
	client     &ReactorClient = unsafe { nil }
	conn       &net.TcpConn   = unsafe { nil }
	text       string
	text_start int
	text_len   int
	opcode     u8
	code       int
	timeout    time.Duration
}

struct ReactorInbox {
mut:
	commands     []ReactorCommand
	payloads     []u8
	bytes        int
	reservations int
}

struct ReactorClosed {
	client &ReactorClient
	code   int
	reason string
}

struct ReactorSocket {
mut:
	client            &ReactorClient = unsafe { nil }
	conn              &net.TcpConn   = unsafe { nil }
	decoder           ServerFrameDecoder
	input             []u8
	output            []u8
	frame_ends        []int
	frame_head        int
	written           int
	write_interest    bool
	read_interest     bool = true
	dirty             bool
	read_scheduled    bool
	closing           bool
	close_received    bool
	abort_after_flush bool
	close_code        int = 1000
	close_reason      string
	read_timeout      time.Duration
	last_read         u64
	write_progress    u64
	close_started     u64
}

// Reactor owns all socket I/O on its run() thread. HTTP validates the upgrade
// and transfers TCP ownership with attach. Create several reactors for multiple
// workers. Existing Client and Server APIs keep their execution model.
@[heap]
pub struct Reactor {
	options  ReactorOptions
	epoll_fd int
	wake_fd  int
mut:
	owner_thread   u64
	serial         u64
	started        u64
	stop_requested u64
	inbox          shared ReactorInbox
	payload_spare  []u8
	sockets        map[u64]&ReactorSocket
	dirty          []u64
	readable       []u64
	closed         []ReactorClosed
	stopping       bool
}

// new_reactor allocates the poller. Call run exactly once, even if stop was
// requested before startup, then join the worker before discarding the reactor.
pub fn new_reactor(options ReactorOptions) !&Reactor {
	if options.max_message_bytes < 1 || options.max_pending_messages < 1
		|| options.max_pending_bytes < 128 || options.max_pending_bytes > 1024 * 1024 * 1024
		|| options.max_connections < 1 || options.max_commands < 1
		|| options.max_command_bytes < 128 || options.frames_per_turn < 1
		|| options.read_timeout <= 0 || options.write_timeout <= 0 || options.close_timeout <= 0 {
		return error('invalid reactor limits')
	}
	ep := C.epoll_create1(C.EPOLL_CLOEXEC)
	if ep < 0 { return error('epoll_create1 failed') }
	wake := C.eventfd(0, C.EFD_CLOEXEC | C.EFD_NONBLOCK)
	if wake < 0 {
		C.close(ep)
		return error('eventfd failed')
	}
	mut event := C.epoll_event{ events: u32(C.EPOLLIN) }
	if C.epoll_ctl(ep, C.EPOLL_CTL_ADD, wake, &event) < 0 {
		C.close(wake)
		C.close(ep)
		return error('epoll wake registration failed')
	}
	return &Reactor{ options: options, epoll_fd: ep, wake_fd: wake }
}

fn (r &Reactor) wake() {
	one := u64(1)
	for C.write(r.wake_fd, &one, 8) < 0 {
		if C.errno != C.EINTR { break }
	}
}

fn (mut r Reactor) post(command ReactorCommand) ! {
	if stdatomic.load_u64(&r.stop_requested) != 0 { return error('reactor stopped') }
	state := stdatomic.load_u64(&command.client.state)
	if command.kind == .close && state >= 2 { return }
	if command.kind != .attach && state >= 2 { return error('connection closing or closed') }
	// Only socket-owner operations can bypass the mailbox. Admission always
	// reserves capacity under the same lock as external producers.
	if command.kind != .attach && stdatomic.load_u64(&r.owner_thread) == u64(C.pthread_self()) {
		if command.kind == .close { stdatomic.store_u64(&command.client.state, 2) }
		r.apply(command)
		return
	}
	lock r.inbox {
		if stdatomic.load_u64(&r.stop_requested) != 0 { return error('reactor stopped') }
		current := stdatomic.load_u64(&command.client.state)
		if command.kind == .close && current >= 2 { return }
		if command.kind != .attach && current >= 2 { return error('connection closing or closed') }
		if command.kind == .attach && r.inbox.reservations >= r.options.max_connections {
			return error('reactor connection limit reached')
		}
		if r.inbox.commands.len >= r.options.max_commands
			|| command.text.len > r.options.max_command_bytes - r.inbox.bytes {
			if command.kind != .attach { stdatomic.store_u64(&command.client.overloaded, 1) }
			r.wake()
			return error('reactor mailbox full')
		}
		if command.kind == .attach { r.inbox.reservations++ }
		if command.kind == .close { stdatomic.store_u64(&command.client.state, 2) }
		was_empty := r.inbox.commands.len == 0
		start := r.inbox.payloads.len
		if command.text.len > 0 {
			unsafe { r.inbox.payloads.push_many(command.text.str, command.text.len) }
		}
		r.inbox.commands << ReactorCommand{
			...command
			text:       ''
			text_start: start
			text_len:   command.text.len
		}
		r.inbox.bytes += command.text.len
		if was_empty { r.wake() }
	}
}

// attach reserves capacity synchronously and transfers conn only on success.
// response is the HTTP 101 response, queued before any WebSocket output.
// Stop accessing conn after success. A later setup failure invokes on_close.
pub fn (mut r Reactor) attach(mut conn net.TcpConn, response string) !&ReactorClient {
	if response.len > r.options.max_pending_bytes { return error('handshake response too large') }
	key := stdatomic.add_u64(&r.serial, 1)
	client := &ReactorClient{ id: '${r.epoll_fd}:${key}', key: key, owner: r }
	r.post(ReactorCommand{ kind: .attach, client: client, conn: conn, text: response })!
	return client
}

// is_open reports whether the worker has attached the connection and no close
// has been requested. It is a snapshot, not a delivery guarantee.
pub fn (c &ReactorClient) is_open() bool {
	return stdatomic.load_u64(&c.state) == 1
}

// write accepts a final text, binary, ping, or pong frame. Control payloads are
// limited to 125 bytes. Cross-thread payloads are copied before returning.
pub fn (mut c ReactorClient) write(payload []u8, opcode OPCode) !int {
	if opcode !in [.text_frame, .binary_frame, .ping, .pong] {
		return error('unsupported output opcode')
	}
	if (opcode in [.ping, .pong] && payload.len > 125)
		|| payload.len > c.owner.options.max_pending_bytes - 10 {
		return error('output frame too large')
	}
	if opcode == .text_frame && !frame_text_valid(payload) {
		return error('invalid text UTF-8')
	}
	// The owner consumes this view inline; external posting copies the bytes.
	text := if payload.len == 0 { '' } else { unsafe { tos(payload.data, payload.len) } }
	c.owner.post(ReactorCommand{ kind: .send, client: c, opcode: u8(opcode), text: text })!
	return payload.len
}

// write_string accepts a final text frame. Success means accepted, not delivered.
pub fn (mut c ReactorClient) write_string(text string) !int {
	return c.write(unsafe { text.str.vbytes(text.len) }, .text_frame)
}

// close starts a bounded closing handshake after accepted output. New sends
// fail once close is accepted. TCP closes after the peer reply or close_timeout.
pub fn (mut c ReactorClient) close(code int, reason string) ! {
	if !frame_close_code_valid(code) || reason.len > 123 || !utf8.validate(reason.str, reason.len) {
		return error('invalid close payload')
	}
	c.owner.post(ReactorCommand{ kind: .close, client: c, code: code, text: reason })!
}

// set_read_timeout changes the connection's read inactivity deadline.
pub fn (mut c ReactorClient) set_read_timeout(timeout time.Duration) ! {
	if timeout <= 0 { return error('timeout must be positive') }
	c.owner.post(ReactorCommand{ kind: .timeout, client: c, timeout: timeout })!
}

// stop rejects new work immediately and requests a bounded graceful shutdown.
// It is idempotent and safe from a callback or another thread. Join run after it.
pub fn (mut r Reactor) stop() {
	lock r.inbox {
		if stdatomic.load_u64(&r.stop_requested) != 0 { return }
		stdatomic.store_u64(&r.stop_requested, 1)
		r.wake()
	}
}

fn (mut r Reactor) terminal(client &ReactorClient, code int, reason string) {
	stdatomic.store_u64(&client.state, 3)
	lock r.inbox {
		r.inbox.reservations--
	}
	// Never invoke close callbacks recursively inside a send or a callback.
	r.closed << ReactorClosed{ client: client, code: code, reason: reason }
}

fn (mut r Reactor) apply(command ReactorCommand) {
	if command.kind == .attach {
		if r.stopping || stdatomic.load_u64(&r.stop_requested) != 0 {
			mut conn := command.conn
			conn.close() or {}
			r.terminal(command.client, 1001, 'Server stopped before attachment')
			return
		}
		mut socket := &ReactorSocket{
			client:       command.client
			conn:         command.conn
			decoder:      ServerFrameDecoder{ max_message_bytes: r.options.max_message_bytes }
			read_timeout: r.options.read_timeout
			last_read:    time.sys_mono_now()
		}
		fd := socket.conn.sock.handle
		flags := C.fcntl(fd, C.F_GETFL, 0)
		mut event := C.epoll_event{ events: u32(C.EPOLLIN | C.EPOLLRDHUP) }
		event.data.u64 = command.client.key
		if flags < 0 || C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK) < 0
			|| C.epoll_ctl(r.epoll_fd, C.EPOLL_CTL_ADD, fd, &event) < 0 {
			socket.conn.close() or {}
			r.terminal(command.client, 1006, 'Socket attachment failed')
			return
		}
		r.sockets[command.client.key] = socket
		socket.output << unsafe { command.text.str.vbytes(command.text.len) }
		socket.write_progress = time.sys_mono_now()
		r.mark_dirty(mut socket)
		// A close may have been queued concurrently before on_open.
		lock r.inbox {
			if stdatomic.load_u64(&command.client.state) == 0 {
				stdatomic.store_u64(&command.client.state, 1)
			}
		}
		r.options.on_open(mut socket.client, r.options.user)
		return
	}
	mut socket := r.sockets[command.client.key] or { return }
	match command.kind {
		.send {
			r.queue_frame(mut socket, command.opcode, unsafe { command.text.str.vbytes(command.text.len) })
		}
		.close { r.begin_close(mut socket, command.code, command.text, false) }
		.timeout { socket.read_timeout = command.timeout }
		else {}
	}
}

fn (mut r Reactor) mark_dirty(mut socket ReactorSocket) {
	if !socket.dirty {
		socket.dirty = true
		r.dirty << socket.client.key
	}
}

fn (mut r Reactor) compact_output(mut socket ReactorSocket) {
	if socket.written > 0 {
		socket.output.delete_many(0, socket.written)
		for i in socket.frame_head .. socket.frame_ends.len {
			socket.frame_ends[i] -= socket.written
		}
		socket.written = 0
	}
	if socket.frame_head > 0 {
		socket.frame_ends.delete_many(0, socket.frame_head)
		socket.frame_head = 0
	}
}

fn (mut r Reactor) queue_frame(mut socket ReactorSocket, opcode u8, payload []u8) {
	if socket.closing && opcode != 8 { return }
	control_close := opcode == 8
	if !control_close && socket.frame_ends.len - socket.frame_head >= r.options.max_pending_messages {
		r.flush(mut socket)
		if socket.client.key !in r.sockets { return }
	}
	limit := r.options.max_pending_bytes + if control_close { 127 } else { 0 }
	header_len := if payload.len < 126 {
		2
	} else if payload.len <= 65535 {
		4
	} else {
		10
	}
	if (!control_close && socket.frame_ends.len - socket.frame_head >= r.options.max_pending_messages)
		|| payload.len > limit - (socket.output.len - socket.written) - header_len {
		r.remove(socket.client.key, 1013, 'Output queue full')
		return
	}
	// Bound retained sent prefixes as well as unsent bytes and reclaim frame slots
	// as individual frames finish, even if the socket never drains completely.
	if socket.written > 0 && (socket.written >= 65536 || socket.output.len + payload.len + header_len > limit) {
		r.compact_output(mut socket)
	} else if socket.frame_head > 0 {
		r.compact_output(mut socket)
	}
	if socket.output.len == socket.written { socket.write_progress = time.sys_mono_now() }
	socket.output << u8(0x80 | opcode)
	if payload.len < 126 {
		socket.output << u8(payload.len)
	} else if payload.len <= 65535 {
		socket.output << [u8(126), u8(payload.len >> 8), u8(payload.len)]
	} else {
		socket.output << u8(127)
		for shift := 56; shift >= 0; shift -= 8 { socket.output << u8(u64(payload.len) >> shift) }
	}
	socket.output << payload
	socket.frame_ends << socket.output.len
	r.mark_dirty(mut socket)
}

fn (mut r Reactor) begin_close(mut socket ReactorSocket, code int, reason string, failed bool) {
	if socket.closing {
		if failed {
			socket.abort_after_flush = true
			socket.close_code = code
			socket.close_reason = reason
			r.mark_dirty(mut socket)
		}
		return
	}
	mut payload := [u8(code >> 8), u8(code)]
	payload << reason.bytes()
	r.queue_frame(mut socket, 8, payload)
	if socket.client.key !in r.sockets { return }
	socket.closing = true
	socket.abort_after_flush = failed
	socket.close_code = code
	socket.close_reason = reason
	socket.close_started = time.sys_mono_now()
	stdatomic.store_u64(&socket.client.state, 2)
}

fn (mut r Reactor) interest(mut socket ReactorSocket, writing bool) {
	reading := !socket.abort_after_flush && !socket.close_received
	if socket.write_interest == writing && socket.read_interest == reading { return }
	socket.write_interest = writing
	socket.read_interest = reading
	mut event := C.epoll_event{
		events: (if reading { u32(C.EPOLLIN | C.EPOLLRDHUP) } else { u32(0) }) | if writing {
			u32(C.EPOLLOUT)
		} else {
			u32(0)
		}
	}
	event.data.u64 = socket.client.key
	if C.epoll_ctl(r.epoll_fd, C.EPOLL_CTL_MOD, socket.conn.sock.handle, &event) < 0 {
		r.remove(socket.client.key, 1006, 'epoll update failed')
	}
}

fn (mut r Reactor) flush(mut socket ReactorSocket) {
	mut sent := 0
	for socket.written < socket.output.len && sent < 65536 {
		count := math.min(socket.output.len - socket.written, 65536 - sent)
		n := C.send(socket.conn.sock.handle, unsafe { &u8(socket.output.data) + socket.written }, count,
			C.MSG_DONTWAIT | C.MSG_NOSIGNAL)
		if n > 0 {
			socket.written += int(n)
			sent += int(n)
			socket.write_progress = time.sys_mono_now()
			for socket.frame_head < socket.frame_ends.len && socket.frame_ends[socket.frame_head] <= socket.written {
				socket.frame_head++
			}
			continue
		}
		if n < 0 && C.errno == C.EINTR { continue }
		if n < 0 && C.errno in [C.EAGAIN, C.EWOULDBLOCK] { break }
		r.remove(socket.client.key, 1006, 'Socket write failed')
		return
	}
	if socket.written < socket.output.len {
		r.interest(mut socket, true)
		return
	}
	socket.output.clear()
	socket.frame_ends.clear()
	socket.written = 0
	socket.frame_head = 0
	if socket.closing && (socket.close_received || socket.abort_after_flush) {
		r.remove(socket.client.key, socket.close_code, socket.close_reason)
	} else {
		r.interest(mut socket, false)
	}
}

fn (mut r Reactor) deliver(mut socket ReactorSocket, frame DecodedFrame) {
	if frame.kind == .message {
		if !socket.closing {
			message := Message{ opcode: frame.opcode, payload: frame.payload }
			r.options.on_message(mut socket.client, &message, r.options.user)
		}
		return
	}
	if frame.opcode == .ping {
		if !socket.closing { r.queue_frame(mut socket, 10, frame.payload) }
	} else if frame.opcode == .close {
		if !socket.closing {
			r.queue_frame(mut socket, 8, frame.payload)
			socket.closing = true
			socket.close_started = time.sys_mono_now()
			socket.close_code = if frame.payload.len >= 2 {
				(int(frame.payload[0]) << 8) | int(frame.payload[1])
			} else {
				1005
			}
			socket.close_reason = if frame.payload.len > 2 {
				frame.payload[2..].bytestr()
			} else {
				''
			}
			stdatomic.store_u64(&socket.client.state, 2)
		}
		socket.close_received = true
		r.mark_dirty(mut socket)
	}
}

fn (mut r Reactor) parse(mut socket ReactorSocket, budget int) int {
	mut offset := 0
	mut frames := 0
	for frames < budget && socket.client.key in r.sockets && !socket.abort_after_flush && !socket.close_received {
		if offset == socket.input.len { break }
		// decode and on_message borrow these bytes only until delivery returns.
		// A tracked slice would force delete_many to detach the reusable input.
		mut input := unsafe {
			(&u8(socket.input.data) + offset).vbytes(socket.input.len - offset)
		}
		frame := socket.decoder.decode(mut input)
		if frame.kind == .need_more { break }
		if frame.kind == .failure {
			r.begin_close(mut socket, frame.close_code, frame.reason, true)
			break
		}
		frames++
		offset += frame.consumed
		if frame.kind in [.message, .control] { r.deliver(mut socket, frame) }
	}
	if offset > 0 { socket.input.delete_many(0, offset) }
	return frames
}

fn (mut r Reactor) read_ready(mut socket ReactorSocket) {
	if socket.abort_after_flush || socket.close_received { return }
	mut buffer := [16384]u8{}
	mut read := 0
	mut frames := r.parse(mut socket, r.options.frames_per_turn)
	for read < 65536 && frames < r.options.frames_per_turn && socket.client.key in r.sockets
		&& !socket.abort_after_flush && !socket.close_received {
		n := C.recv(socket.conn.sock.handle, &buffer[0], buffer.len, C.MSG_DONTWAIT)
		if n > 0 {
			read += int(n)
			socket.last_read = time.sys_mono_now()
			// recv initialized exactly n bytes. Copy them before this stack buffer is reused.
			unsafe { socket.input.push_many(&buffer[0], int(n)) }
			frames += r.parse(mut socket, r.options.frames_per_turn - frames)
			continue
		}
		if n < 0 && C.errno == C.EINTR { continue }
		if n < 0 && C.errno in [C.EAGAIN, C.EWOULDBLOCK] { break }
		r.remove(socket.client.key, 1006, 'Socket read ended')
		return
	}
	if frames >= r.options.frames_per_turn && socket.input.len > 0 && !socket.read_scheduled
		&& socket.client.key in r.sockets && !socket.abort_after_flush && !socket.close_received {
		socket.read_scheduled = true
		r.readable << socket.client.key
	}
}

fn (mut r Reactor) remove(key u64, code int, reason string) {
	mut socket := r.sockets[key] or { return }
	r.sockets.delete(key)
	C.epoll_ctl(r.epoll_fd, C.EPOLL_CTL_DEL, socket.conn.sock.handle, unsafe { nil })
	socket.conn.close() or {}
	r.terminal(socket.client, code, reason)
}

fn (mut r Reactor) drain_inbox() {
	mut commands := []ReactorCommand{}
	mut payloads := []u8{}
	lock r.inbox {
		if r.inbox.commands.len == 0 { return }
		commands = r.inbox.commands
		r.inbox.commands = []
		payloads = r.inbox.payloads
		r.inbox.payloads = r.payload_spare
		r.inbox.bytes = 0
	}
	for command in commands {
		text := if command.text_len > 0 {
			// The detached batch stays unchanged until every command is applied.
			unsafe { (&u8(payloads.data) + command.text_start).vstring_literal_with_len(command.text_len) }
		} else {
			''
		}
		// Close reasons may outlive this batch; sends and upgrade responses are copied by apply.
		r.apply(ReactorCommand{
			...command
			text: if command.kind == .close {
				text.clone()
			} else {
				text
			}
		})
	}
	payloads.clear()
	r.payload_spare = payloads
}

fn (mut r Reactor) notify_closed() {
	events := r.closed
	r.closed = []
	for event in events {
		mut client := event.client
		r.options.on_close(mut client, event.code, event.reason, r.options.user)
	}
}

fn (mut r Reactor) sweep(now u64) {
	for key in r.sockets.keys() {
		mut socket := r.sockets[key] or { continue }
		if stdatomic.load_u64(&socket.client.overloaded) != 0 {
			r.remove(key, 1013, 'Mailbox full')
		} else if socket.closing && now - socket.close_started >= u64(r.options.close_timeout) {
			r.remove(key, 1006, 'Close handshake timed out')
		} else if socket.output.len > socket.written && now - socket.write_progress >= u64(r.options.write_timeout) {
			r.remove(key, 1006, 'Write timed out')
		} else if !socket.closing && now - socket.last_read >= u64(socket.read_timeout) {
			r.remove(key, 1006, 'Read timed out')
		}
	}
}

// run executes exactly once and releases all descriptors before returning.
// Monotonic IDs protect descriptor reuse. Reads, frames, and writes have per-turn
// budgets; partial sends resume at their byte offset. Deadline sweeps are 50 ms.
pub fn (mut r Reactor) run() ! {
	if stdatomic.fetch_add_u64(&r.started, 1) != 0 { return error('reactor already started') }
	stdatomic.store_u64(&r.owner_thread, u64(C.pthread_self()))
	defer {
		lock r.inbox {
			stdatomic.store_u64(&r.stop_requested, 1)
		}
		r.stopping = true
		r.drain_inbox()
		for key in r.sockets.keys() { r.remove(key, 1006, 'Reactor exited') }
		r.notify_closed()
		lock r.inbox {
			C.close(r.wake_fd)
			C.close(r.epoll_fd)
		}
		stdatomic.store_u64(&r.owner_thread, 0)
	}
	mut events := [256]C.epoll_event{}
	mut last_sweep := time.sys_mono_now()
	for {
		r.drain_inbox()
		if !r.stopping && stdatomic.load_u64(&r.stop_requested) != 0 {
			r.stopping = true
			for key in r.sockets.keys() {
				mut socket := r.sockets[key] or { continue }
				r.begin_close(mut socket, 1001, 'Server stopped', false)
			}
		}
		reads := r.readable
		r.readable = []
		for key in reads {
			mut socket := r.sockets[key] or { continue }
			socket.read_scheduled = false
			r.read_ready(mut socket)
		}
		ready := r.dirty
		r.dirty = []
		for key in ready {
			mut socket := r.sockets[key] or { continue }
			socket.dirty = false
			r.flush(mut socket)
		}
		r.notify_closed()
		if r.stopping && r.sockets.len == 0 { return }
		timeout := if r.dirty.len > 0 || r.readable.len > 0 || r.closed.len > 0 { 0 } else { 50 }
		count := C.epoll_wait(r.epoll_fd, &events[0], events.len, timeout)
		if count < 0 {
			if C.errno == C.EINTR { continue }
			return error('epoll_wait failed')
		}
		for i in 0 .. count {
			key := unsafe { events[i].data.u64 }
			if key == 0 {
				mut value := u64(0)
				for { if C.read(r.wake_fd, &value, 8) >= 0 || C.errno != C.EINTR { break } }
				continue
			}
			mut socket := r.sockets[key] or { continue }
			if events[i].events & u32(C.EPOLLIN | C.EPOLLRDHUP | C.EPOLLHUP) != 0 {
				r.read_ready(mut socket)
			}
			if key !in r.sockets { continue }
			if events[i].events & u32(C.EPOLLOUT) != 0 { r.flush(mut socket) }
			if key in r.sockets && events[i].events & u32(C.EPOLLERR) != 0 {
				r.remove(key, 1006, 'Socket error')
			}
		}
		now := time.sys_mono_now()
		if now - last_sweep >= 50 * time.millisecond {
			last_sweep = now
			r.sweep(now)
		}
	}
}
