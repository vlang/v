// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Channel implementation for goroutines.
// Translated from Go's runtime/chan.go.
//
// Channels provide goroutine-safe communication between goroutines.
// They support both buffered and unbuffered modes.
//
// Key operations translated from Go:
//   - makechan()  -> chan_make()
//   - chansend()  -> chan_send()
//   - chanrecv()  -> chan_recv()
//   - closechan() -> chan_close()
module goroutines

import sync

// Chan is a goroutine-safe channel for communication between goroutines.
// Translated from Go's hchan struct in chan.go.
pub struct Chan {
pub mut:
	mu       sync.Mutex // protects all fields
	qcount   u32        // total data in the queue
	dataqsiz u32        // size of the circular buffer
	buf      voidptr    // circular buffer for buffered channels
	elemsize u16        // size of each element
	closed   bool       // true if channel is closed

	sendx u32 // send index into circular buffer
	recvx u32 // receive index into circular buffer

	recvq WaitQ // list of recv waiters
	sendq WaitQ // list of send waiters
}

// chan_make creates a new channel.
// If buf_size > 0, creates a buffered channel.
// Translated from Go's makechan() in chan.go.
pub fn chan_make(elem_size int, buf_size int) &Chan {
	mut c := &Chan{
		elemsize: u16(elem_size)
		dataqsiz: u32(buf_size)
	}
	if buf_size > 0 {
		c.buf = unsafe { malloc(elem_size * buf_size) }
	}
	return c
}

// chan_send sends a value on the channel.
// If block is true, blocks until the send can proceed.
// Returns true if the value was sent.
// Translated from Go's chansend() in chan.go.
pub fn chan_send(c &Chan, ep voidptr, block bool) bool {
	if c == unsafe { nil } {
		if !block {
			return false
		}
		// Block forever on nil channel (Go behavior)
		gopark('chan send (nil chan)')
		return false // unreachable
	}

	mut ch := unsafe { c }
	ch.mu.@lock()

	if ch.closed {
		ch.mu.unlock()
		panic('send on closed channel')
	}

	// Fast path: try to find a waiting receiver
	sg := ch.recvq.dequeue()
	if sg != unsafe { nil } {
		// Found a waiting receiver - send directly
		ch.mu.unlock()
		send_direct(sg, ep, ch.elemsize)
		return true
	}

	// Buffered channel with space available
	if ch.qcount < ch.dataqsiz {
		// Put data in buffer
		dst := chan_buf(ch, ch.sendx)
		unsafe { C.memcpy(dst, ep, ch.elemsize) }
		ch.sendx++
		if ch.sendx == ch.dataqsiz {
			ch.sendx = 0
		}
		ch.qcount++
		ch.mu.unlock()
		return true
	}

	if !block {
		ch.mu.unlock()
		return false
	}

	// Block: enqueue ourselves on the send wait queue
	gp := get_current_g()
	mut mysg := &Sudog{
		g:    unsafe { gp }
		elem: ep
		c:    voidptr(ch)
	}
	ch.sendq.enqueue(mysg)
	ch.mu.unlock()

	// Park the goroutine until a receiver wakes us
	gopark('chan send')

	return true
}

// chan_recv receives a value from the channel.
// If block is true, blocks until a value is available.
// Returns (received, ok). ok is false if channel is closed and empty.
// Translated from Go's chanrecv() in chan.go.
pub fn chan_recv(c &Chan, ep voidptr, block bool) (bool, bool) {
	if c == unsafe { nil } {
		if !block {
			return false, false
		}
		gopark('chan receive (nil chan)')
		return false, false // unreachable
	}

	mut ch := unsafe { c }
	ch.mu.@lock()

	// Fast path: try to find a waiting sender
	sg := ch.sendq.dequeue()
	if sg != unsafe { nil } {
		ch.mu.unlock()
		recv_direct(ch, sg, ep)
		return true, true
	}

	// Buffered channel with data available
	if ch.qcount > 0 {
		src := chan_buf(ch, ch.recvx)
		if ep != unsafe { nil } {
			unsafe { C.memcpy(ep, src, ch.elemsize) }
		}
		ch.recvx++
		if ch.recvx == ch.dataqsiz {
			ch.recvx = 0
		}
		ch.qcount--
		ch.mu.unlock()
		return true, true
	}

	if ch.closed {
		ch.mu.unlock()
		if ep != unsafe { nil } {
			unsafe { C.memset(ep, 0, ch.elemsize) }
		}
		return true, false
	}

	if !block {
		ch.mu.unlock()
		return false, false
	}

	// Block: enqueue ourselves on the recv wait queue
	gp := get_current_g()
	mut mysg := &Sudog{
		g:    unsafe { gp }
		elem: ep
		c:    voidptr(ch)
	}
	ch.recvq.enqueue(mysg)
	ch.mu.unlock()

	// Park until a sender wakes us
	gopark('chan receive')

	return true, true
}

// chan_close closes the channel.
// Translated from Go's closechan() in chan.go.
pub fn chan_close(c &Chan) {
	if c == unsafe { nil } {
		panic('close of nil channel')
	}

	mut ch := unsafe { c }
	ch.mu.@lock()

	if ch.closed {
		ch.mu.unlock()
		panic('close of closed channel')
	}

	ch.closed = true

	// Wake all waiting receivers
	for {
		sg := ch.recvq.dequeue()
		if sg == unsafe { nil } {
			break
		}
		if sg.elem != unsafe { nil } {
			unsafe { C.memset(sg.elem, 0, ch.elemsize) }
		}
		sg.success = false
		goready(sg.g)
	}

	// Wake all waiting senders (they will panic)
	for {
		sg := ch.sendq.dequeue()
		if sg == unsafe { nil } {
			break
		}
		sg.success = false
		goready(sg.g)
	}

	ch.mu.unlock()
}

// send_direct sends data directly from sender to a waiting receiver.
// Translated from Go's send() in chan.go.
fn send_direct(sg &Sudog, ep voidptr, elem_size u16) {
	if sg.elem != unsafe { nil } {
		unsafe { C.memcpy(sg.elem, ep, elem_size) }
	}
	mut s := unsafe { sg }
	s.success = true
	goready(sg.g)
}

// recv_direct receives data directly from a waiting sender.
fn recv_direct(ch &Chan, sg &Sudog, ep voidptr) {
	if ch.dataqsiz == 0 {
		// Unbuffered: copy directly from sender
		if ep != unsafe { nil } {
			unsafe { C.memcpy(ep, sg.elem, ch.elemsize) }
		}
	} else {
		// Buffered: take from buffer, then copy sender's data into buffer
		buf_elem := chan_buf(ch, unsafe { ch }.recvx)
		if ep != unsafe { nil } {
			unsafe { C.memcpy(ep, buf_elem, ch.elemsize) }
		}
		unsafe { C.memcpy(buf_elem, sg.elem, ch.elemsize) }
		unsafe {
			ch.recvx++
			if ch.recvx == ch.dataqsiz {
				ch.recvx = 0
			}
			ch.sendx = ch.recvx
		}
	}
	mut s := unsafe { sg }
	s.success = true
	goready(sg.g)
}

// chan_buf returns a pointer to the i-th slot in the buffer.
// Translated from Go's chanbuf() in chan.go.
fn chan_buf(c &Chan, i u32) voidptr {
	return unsafe { voidptr(usize(c.buf) + usize(i) * usize(c.elemsize)) }
}

// chan_len returns the number of elements in the channel buffer.
pub fn chan_len(c &Chan) int {
	if c == unsafe { nil } {
		return 0
	}
	return int(c.qcount)
}

// chan_cap returns the capacity of the channel buffer.
pub fn chan_cap(c &Chan) int {
	if c == unsafe { nil } {
		return 0
	}
	return int(c.dataqsiz)
}
