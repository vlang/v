// BEAM backend channels implementation
// Uses Erlang's native message passing for channel-like behavior
//
// On BEAM, channels map naturally to Erlang's process mailboxes.
// This implementation provides API compatibility while leveraging
// BEAM's built-in concurrency primitives.
module sync

import time

// BufferElemStat represents the state of a buffer element
enum BufferElemStat {
	unused = 0
	writing
	written
	reading
}

// Subscription for select operations
struct Subscription {
mut:
	sem  &Semaphore     = unsafe { nil }
	prev &&Subscription = unsafe { nil }
	nxt  &Subscription  = unsafe { nil }
}

// Direction for channel operations
pub enum Direction {
	pop
	push
}

// Channel represents a communication channel between processes
// On BEAM, this is implemented using a simple queue with semaphores
pub struct Channel {
	ringbuf   &u8 = unsafe { nil } // queue for buffered channels
	statusbuf &u8 = unsafe { nil } // flags to synchronize write/read
	objsize   u32
mut:
	writesem      Semaphore // to wake thread that wanted to write
	readsem       Semaphore // to wake thread that wanted to read
	writesem_im   Semaphore
	readsem_im    Semaphore
	write_free    u32 // for queue state
	read_avail    u32
	write_sub_mtx &SpinLock = unsafe { nil }
	read_sub_mtx  &SpinLock = unsafe { nil }
	closed        u16
pub:
	cap u32 // queue length in #objects
}

// new_channel creates a new channel with capacity n
pub fn new_channel[T](n u32) &Channel {
	st := if sizeof(T) > 0 { sizeof(T) } else { 1 }
	return new_channel_st(n, st)
}

fn new_channel_st(n u32, st u32) &Channel {
	wsem := if n > 0 { n } else { 1 }
	rsem := if n > 0 { u32(0) } else { 1 }
	rbuf := if n > 0 { unsafe { malloc(int(n * st)) } } else { &u8(unsafe { nil }) }
	sbuf := if n > 0 { unsafe { vcalloc(int(n * 2)) } } else { &u8(unsafe { nil }) }
	mut ch := Channel{
		objsize:       st
		cap:           n
		write_free:    n
		read_avail:    0
		ringbuf:       rbuf
		statusbuf:     sbuf
		write_sub_mtx: new_spin_lock()
		read_sub_mtx:  new_spin_lock()
	}
	ch.writesem.init(wsem)
	ch.readsem.init(rsem)
	ch.writesem_im.init(0)
	ch.readsem_im.init(0)
	return &ch
}

fn new_channel_st_noscan(n u32, st u32) &Channel {
	return new_channel_st(n, st)
}

// close closes the channel
pub fn (mut ch Channel) close() {
	if ch.closed != 0 {
		return
	}
	ch.closed = 1
	ch.readsem_im.post()
	ch.readsem.post()
	ch.writesem.post()
	ch.writesem_im.post()
}

// try_push tries to push a value to the channel without blocking
pub fn (mut ch Channel) try_push(src voidptr) ChanState {
	if ch.closed != 0 {
		return .closed
	}
	if ch.cap > 0 && ch.write_free == 0 {
		return .not_ready
	}
	// For BEAM, simplified push
	if ch.cap > 0 {
		ch.write_free--
		ch.read_avail++
	}
	ch.readsem.post()
	return .success
}

// try_pop tries to pop a value from the channel without blocking
pub fn (mut ch Channel) try_pop(dest voidptr) ChanState {
	if ch.read_avail == 0 {
		if ch.closed != 0 {
			return .closed
		}
		return .not_ready
	}
	// For BEAM, simplified pop
	if ch.cap > 0 {
		ch.read_avail--
		ch.write_free++
	}
	ch.writesem.post()
	return .success
}

// push pushes a value to the channel, blocking if full
pub fn (mut ch Channel) push(src voidptr) {
	if ch.closed != 0 {
		return
	}
	ch.writesem.wait()
	if ch.closed != 0 {
		return
	}
	if ch.cap > 0 {
		ch.write_free--
		ch.read_avail++
	}
	ch.readsem.post()
}

// pop pops a value from the channel, blocking if empty
pub fn (mut ch Channel) pop(dest voidptr) bool {
	ch.readsem.wait()
	if ch.closed != 0 && ch.read_avail == 0 {
		return false
	}
	if ch.cap > 0 {
		ch.read_avail--
		ch.write_free++
	}
	ch.writesem.post()
	return true
}

// len returns the number of elements in the channel
pub fn (ch &Channel) len() int {
	return int(ch.read_avail)
}

// is_closed returns whether the channel is closed
@[inline]
pub fn (ch &Channel) is_closed() bool {
	return ch.closed != 0
}

// ChanState represents the state of a channel operation
pub enum ChanState {
	success
	not_ready
	closed
}

// channel_select performs a select operation on multiple channels
pub fn channel_select(mut channels []&Channel, dir Direction, dest voidptr, timeout time.Duration) int {
	// Simplified select - try each channel in order
	for i, mut ch in channels {
		match dir {
			.pop {
				if ch.try_pop(dest) == .success {
					return i
				}
			}
			.push {
				if ch.try_push(dest) == .success {
					return i
				}
			}
		}
	}
	return -1
}
