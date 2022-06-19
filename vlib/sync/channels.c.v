module sync

import time
import rand
import sync.stdatomic

const aops_used = stdatomic.used

const (
	// how often to try to get data without blocking before to wait for semaphore
	spinloops     = 750
	spinloops_sem = 4000
)

enum BufferElemStat {
	unused = 0
	writing
	written
	reading
}

struct Subscription {
mut:
	sem  &Semaphore
	prev &&Subscription
	nxt  &Subscription
}

enum Direction {
	pop
	push
}

pub struct Channel {
	ringbuf   &u8 // queue for buffered channels
	statusbuf &u8 // flags to synchronize write/read in ringbuf
	objsize   u32
mut: // atomic
	writesem           Semaphore // to wake thread that wanted to write, but buffer was full
	readsem            Semaphore // to wake thread that wanted to read, but buffer was empty
	writesem_im        Semaphore
	readsem_im         Semaphore
	write_adr          C.atomic_uintptr_t // if != NULL the next obj can be written here without wait
	read_adr           C.atomic_uintptr_t // if != NULL an obj can be read from here without wait
	adr_read           C.atomic_uintptr_t // used to identify origin of writesem
	adr_written        C.atomic_uintptr_t // used to identify origin of readsem
	write_free         u32 // for queue state
	read_avail         u32
	buf_elem_write_idx u32
	buf_elem_read_idx  u32
	// for select
	write_subscriber &Subscription
	read_subscriber  &Subscription
	write_sub_mtx    u16
	read_sub_mtx     u16
	closed           u16
pub:
	cap u32 // queue length in #objects
}

pub fn new_channel<T>(n u32) &Channel {
	st := sizeof(T)
	if isreftype(T) {
		return new_channel_st(n, st)
	} else {
		return new_channel_st_noscan(n, st)
	}
}

fn new_channel_st(n u32, st u32) &Channel {
	wsem := if n > 0 { n } else { 1 }
	rsem := if n > 0 { u32(0) } else { 1 }
	rbuf := if n > 0 { unsafe { malloc(int(n * st)) } } else { &u8(0) }
	sbuf := if n > 0 { vcalloc_noscan(int(n * 2)) } else { &u8(0) }
	mut ch := Channel{
		objsize: st
		cap: n
		write_free: n
		read_avail: 0
		ringbuf: rbuf
		statusbuf: sbuf
		write_subscriber: 0
		read_subscriber: 0
	}
	ch.writesem.init(wsem)
	ch.readsem.init(rsem)
	ch.writesem_im.init(0)
	ch.readsem_im.init(0)
	return &ch
}

fn new_channel_st_noscan(n u32, st u32) &Channel {
	$if gcboehm_opt ? {
		wsem := if n > 0 { n } else { 1 }
		rsem := if n > 0 { u32(0) } else { 1 }
		rbuf := if n > 0 { unsafe { malloc_noscan(int(n * st)) } } else { &u8(0) }
		sbuf := if n > 0 { vcalloc_noscan(int(n * 2)) } else { &u8(0) }
		mut ch := Channel{
			objsize: st
			cap: n
			write_free: n
			read_avail: 0
			ringbuf: rbuf
			statusbuf: sbuf
			write_subscriber: 0
			read_subscriber: 0
		}
		ch.writesem.init(wsem)
		ch.readsem.init(rsem)
		ch.writesem_im.init(0)
		ch.readsem_im.init(0)
		return &ch
	} $else {
		return new_channel_st(n, st)
	}
}

pub fn (ch &Channel) auto_str(typename string) string {
	return 'chan $typename{cap: $ch.cap, closed: $ch.closed}'
}

pub fn (mut ch Channel) close() {
	open_val := u16(0)
	if !C.atomic_compare_exchange_strong_u16(&ch.closed, &open_val, 1) {
		return
	}
	mut nulladr := voidptr(0)
	for !C.atomic_compare_exchange_weak_ptr(unsafe { &voidptr(&ch.adr_written) }, &nulladr,
		voidptr(-1)) {
		nulladr = voidptr(0)
	}
	ch.readsem_im.post()
	ch.readsem.post()
	mut null16 := u16(0)
	for !C.atomic_compare_exchange_weak_u16(&ch.read_sub_mtx, &null16, u16(1)) {
		null16 = u16(0)
	}
	if ch.read_subscriber != voidptr(0) {
		ch.read_subscriber.sem.post()
	}
	C.atomic_store_u16(&ch.read_sub_mtx, u16(0))
	null16 = u16(0)
	for !C.atomic_compare_exchange_weak_u16(&ch.write_sub_mtx, &null16, u16(1)) {
		null16 = u16(0)
	}
	if ch.write_subscriber != voidptr(0) {
		ch.write_subscriber.sem.post()
	}
	C.atomic_store_u16(&ch.write_sub_mtx, u16(0))
	ch.writesem.post()
	if ch.cap == 0 {
		C.atomic_store_ptr(unsafe { &voidptr(&ch.read_adr) }, voidptr(0))
	}
	ch.writesem_im.post()
}

[inline]
pub fn (mut ch Channel) len() int {
	return int(C.atomic_load_u32(&ch.read_avail))
}

[inline]
pub fn (mut ch Channel) closed() bool {
	return C.atomic_load_u16(&ch.closed) != 0
}

[inline]
pub fn (mut ch Channel) push(src voidptr) {
	if ch.try_push_priv(src, false) == .closed {
		panic('push on closed channel')
	}
}

[inline]
pub fn (mut ch Channel) try_push(src voidptr) ChanState {
	return ch.try_push_priv(src, true)
}

fn (mut ch Channel) try_push_priv(src voidptr, no_block bool) ChanState {
	if C.atomic_load_u16(&ch.closed) != 0 {
		return .closed
	}
	spinloops_sem_, spinloops_ := if no_block { 1, 1 } else { sync.spinloops, sync.spinloops_sem }
	mut have_swapped := false
	for {
		mut got_sem := false
		mut wradr := C.atomic_load_ptr(unsafe { &voidptr(&ch.write_adr) })
		for wradr != C.NULL {
			if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.write_adr) },
				&wradr, voidptr(0))
			{
				// there is a reader waiting for us
				unsafe { C.memcpy(wradr, src, ch.objsize) }
				mut nulladr := voidptr(0)
				for !C.atomic_compare_exchange_weak_ptr(unsafe { &voidptr(&ch.adr_written) },
					&nulladr, wradr) {
					nulladr = voidptr(0)
				}
				ch.readsem_im.post()
				return .success
			}
		}
		if no_block && ch.cap == 0 {
			return .not_ready
		}
		// get token to read
		for _ in 0 .. spinloops_sem_ {
			if got_sem {
				break
			}
			got_sem = ch.writesem.try_wait()
		}
		if !got_sem {
			if no_block {
				return .not_ready
			}
			ch.writesem.wait()
		}
		if C.atomic_load_u16(&ch.closed) != 0 {
			ch.writesem.post()
			return .closed
		}
		if ch.cap == 0 {
			// try to advertise current object as readable
			mut read_in_progress := false
			C.atomic_store_ptr(unsafe { &voidptr(&ch.read_adr) }, src)
			wradr = C.atomic_load_ptr(unsafe { &voidptr(&ch.write_adr) })
			if wradr != C.NULL {
				mut src2 := src
				if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.read_adr) },
					&src2, voidptr(0))
				{
					ch.writesem.post()
					continue
				} else {
					read_in_progress = true
				}
			}
			if !read_in_progress {
				mut null16 := u16(0)
				for !C.atomic_compare_exchange_weak_u16(voidptr(&ch.read_sub_mtx), &null16,
					u16(1)) {
					null16 = u16(0)
				}
				if ch.read_subscriber != voidptr(0) {
					ch.read_subscriber.sem.post()
				}
				C.atomic_store_u16(&ch.read_sub_mtx, u16(0))
			}
			mut src2 := src
			for sp := u32(0); sp < spinloops_ || read_in_progress; sp++ {
				if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.adr_read) },
					&src2, voidptr(0))
				{
					have_swapped = true
					read_in_progress = true
					break
				}
				src2 = src
			}
			mut got_im_sem := false
			for sp := u32(0); sp < spinloops_sem_ || read_in_progress; sp++ {
				got_im_sem = ch.writesem_im.try_wait()
				if got_im_sem {
					break
				}
			}
			for {
				if got_im_sem {
					got_im_sem = false
				} else {
					ch.writesem_im.wait()
				}
				if C.atomic_load_u16(&ch.closed) != 0 {
					if have_swapped
						|| C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.adr_read) }, &src2, voidptr(0)) {
						ch.writesem.post()
						return .success
					} else {
						return .closed
					}
				}
				if have_swapped
					|| C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.adr_read) }, &src2, voidptr(0)) {
					ch.writesem.post()
					break
				} else {
					// this semaphore was not for us - repost in
					ch.writesem_im.post()
					if src2 == voidptr(-1) {
						ch.readsem.post()
						return .closed
					}
					src2 = src
				}
			}
			return .success
		} else {
			// buffered channel
			mut space_in_queue := false
			mut wr_free := C.atomic_load_u32(&ch.write_free)
			for wr_free > 0 {
				space_in_queue = C.atomic_compare_exchange_weak_u32(&ch.write_free, &wr_free,
					wr_free - 1)
				if space_in_queue {
					break
				}
			}
			if space_in_queue {
				mut wr_idx := C.atomic_load_u32(&ch.buf_elem_write_idx)
				for {
					mut new_wr_idx := wr_idx + 1
					for new_wr_idx >= ch.cap {
						new_wr_idx -= ch.cap
					}
					if C.atomic_compare_exchange_strong_u32(&ch.buf_elem_write_idx, &wr_idx,
						new_wr_idx)
					{
						break
					}
				}
				mut wr_ptr := ch.ringbuf
				mut status_adr := ch.statusbuf
				unsafe {
					wr_ptr += (wr_idx * ch.objsize)
					status_adr += wr_idx * sizeof(u16)
				}
				mut expected_status := u16(BufferElemStat.unused)
				for !C.atomic_compare_exchange_weak_u16(status_adr, &expected_status,
					u16(BufferElemStat.writing)) {
					expected_status = u16(BufferElemStat.unused)
				}
				unsafe {
					C.memcpy(wr_ptr, src, ch.objsize)
				}
				C.atomic_store_u16(unsafe { &u16(status_adr) }, u16(BufferElemStat.written))
				C.atomic_fetch_add_u32(&ch.read_avail, 1)
				ch.readsem.post()
				mut null16 := u16(0)
				for !C.atomic_compare_exchange_weak_u16(&ch.read_sub_mtx, &null16, u16(1)) {
					null16 = u16(0)
				}
				if ch.read_subscriber != voidptr(0) {
					ch.read_subscriber.sem.post()
				}
				C.atomic_store_u16(&ch.read_sub_mtx, u16(0))
				return .success
			} else {
				if no_block {
					return .not_ready
				}
				ch.writesem.post()
			}
		}
	}
	// we should not get here but the V compiler want's to see a return statement
	panic('unknown `try_push_priv` state')
}

[inline]
pub fn (mut ch Channel) pop(dest voidptr) bool {
	return ch.try_pop_priv(dest, false) == .success
}

[inline]
pub fn (mut ch Channel) try_pop(dest voidptr) ChanState {
	return ch.try_pop_priv(dest, true)
}

fn (mut ch Channel) try_pop_priv(dest voidptr, no_block bool) ChanState {
	spinloops_sem_, spinloops_ := if no_block { 1, 1 } else { sync.spinloops, sync.spinloops_sem }
	mut have_swapped := false
	mut write_in_progress := false
	for {
		mut got_sem := false
		if ch.cap == 0 {
			// unbuffered channel - first see if a `push()` has adversized
			mut rdadr := C.atomic_load_ptr(unsafe { &voidptr(&ch.read_adr) })
			for rdadr != C.NULL {
				if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.read_adr) },
					&rdadr, voidptr(0))
				{
					// there is a writer waiting for us
					unsafe { C.memcpy(dest, rdadr, ch.objsize) }
					mut nulladr := voidptr(0)
					for !C.atomic_compare_exchange_weak_ptr(unsafe { &voidptr(&ch.adr_read) },
						&nulladr, rdadr) {
						nulladr = voidptr(0)
					}
					ch.writesem_im.post()
					return .success
				}
			}
			if no_block {
				if C.atomic_load_u16(&ch.closed) == 0 {
					return .not_ready
				} else {
					return .closed
				}
			}
		}
		// get token to read
		for _ in 0 .. spinloops_sem_ {
			if got_sem {
				break
			}
			got_sem = ch.readsem.try_wait()
		}
		if !got_sem {
			if no_block {
				if C.atomic_load_u16(&ch.closed) == 0 {
					return .not_ready
				} else {
					return .closed
				}
			}
			ch.readsem.wait()
		}
		if ch.cap > 0 {
			// try to get buffer token
			mut obj_in_queue := false
			mut rd_avail := C.atomic_load_u32(&ch.read_avail)
			for rd_avail > 0 {
				obj_in_queue = C.atomic_compare_exchange_weak_u32(&ch.read_avail, &rd_avail,
					rd_avail - 1)
				if obj_in_queue {
					break
				}
			}
			if obj_in_queue {
				mut rd_idx := C.atomic_load_u32(&ch.buf_elem_read_idx)
				for {
					mut new_rd_idx := rd_idx + 1
					for new_rd_idx >= ch.cap {
						new_rd_idx -= ch.cap
					}
					if C.atomic_compare_exchange_weak_u32(&ch.buf_elem_read_idx, &rd_idx,
						new_rd_idx)
					{
						break
					}
				}
				mut rd_ptr := ch.ringbuf
				mut status_adr := ch.statusbuf
				unsafe {
					rd_ptr += rd_idx * ch.objsize
					status_adr += rd_idx * sizeof(u16)
				}
				mut expected_status := u16(BufferElemStat.written)
				for !C.atomic_compare_exchange_weak_u16(status_adr, &expected_status,
					u16(BufferElemStat.reading)) {
					expected_status = u16(BufferElemStat.written)
				}
				unsafe {
					C.memcpy(dest, rd_ptr, ch.objsize)
				}
				C.atomic_store_u16(unsafe { &u16(status_adr) }, u16(BufferElemStat.unused))
				C.atomic_fetch_add_u32(&ch.write_free, 1)
				ch.writesem.post()
				mut null16 := u16(0)
				for !C.atomic_compare_exchange_weak_u16(&ch.write_sub_mtx, &null16, u16(1)) {
					null16 = u16(0)
				}
				if ch.write_subscriber != voidptr(0) {
					ch.write_subscriber.sem.post()
				}
				C.atomic_store_u16(&ch.write_sub_mtx, u16(0))
				return .success
			}
		}
		// try to advertise `dest` as writable
		C.atomic_store_ptr(unsafe { &voidptr(&ch.write_adr) }, dest)
		if ch.cap == 0 {
			mut rdadr := C.atomic_load_ptr(unsafe { &voidptr(&ch.read_adr) })
			if rdadr != C.NULL {
				mut dest2 := dest
				if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.write_adr) },
					&dest2, voidptr(0))
				{
					ch.readsem.post()
					continue
				} else {
					write_in_progress = true
				}
			}
		}
		if ch.cap == 0 && !write_in_progress {
			mut null16 := u16(0)
			for !C.atomic_compare_exchange_weak_u16(&ch.write_sub_mtx, &null16, u16(1)) {
				null16 = u16(0)
			}
			if ch.write_subscriber != voidptr(0) {
				ch.write_subscriber.sem.post()
			}
			C.atomic_store_u16(&ch.write_sub_mtx, u16(0))
		}
		mut dest2 := dest
		for sp := u32(0); sp < spinloops_ || write_in_progress; sp++ {
			if C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.adr_written) },
				&dest2, voidptr(0))
			{
				have_swapped = true
				break
			} else if dest2 == voidptr(-1) {
				ch.readsem.post()
				return .closed
			}
			dest2 = dest
		}
		mut got_im_sem := false
		for sp := u32(0); sp < spinloops_sem_ || write_in_progress; sp++ {
			got_im_sem = ch.readsem_im.try_wait()
			if got_im_sem {
				break
			}
		}
		for {
			if got_im_sem {
				got_im_sem = false
			} else {
				ch.readsem_im.wait()
			}
			if have_swapped
				|| C.atomic_compare_exchange_strong_ptr(unsafe { &voidptr(&ch.adr_written) }, &dest2, voidptr(0)) {
				ch.readsem.post()
				break
			} else {
				// this semaphore was not for us - repost in
				ch.readsem_im.post()
				if dest2 == voidptr(-1) {
					ch.readsem.post()
					return .closed
				}
				dest2 = dest
			}
		}
		break
	}
	return .success
}

// Wait `timeout` on any of `channels[i]` until one of them can push (`is_push[i] = true`) or pop (`is_push[i] = false`)
// object referenced by `objrefs[i]`. `timeout = time.infinite` means wait unlimited time. `timeout <= 0` means return
// immediately if no transaction can be performed without waiting.
// return value: the index of the channel on which a transaction has taken place
//               -1 if waiting for a transaction has exceeded timeout
//               -2 if all channels are closed

pub fn channel_select(mut channels []&Channel, dir []Direction, mut objrefs []voidptr, timeout time.Duration) int {
	$if debug {
		assert channels.len == dir.len
		assert dir.len == objrefs.len
	}
	mut subscr := []Subscription{len: channels.len}
	mut sem := unsafe { Semaphore{} }
	sem.init(0)
	for i, ch in channels {
		subscr[i].sem = unsafe { &sem }
		if dir[i] == .push {
			mut null16 := u16(0)
			for !C.atomic_compare_exchange_weak_u16(&ch.write_sub_mtx, &null16, u16(1)) {
				null16 = u16(0)
			}
			subscr[i].prev = unsafe { &ch.write_subscriber }
			unsafe {
				subscr[i].nxt = &Subscription(C.atomic_exchange_ptr(&voidptr(&ch.write_subscriber),
					&subscr[i]))
			}
			if voidptr(subscr[i].nxt) != voidptr(0) {
				subscr[i].nxt.prev = unsafe { &subscr[i].nxt }
			}
			C.atomic_store_u16(&ch.write_sub_mtx, u16(0))
		} else {
			mut null16 := u16(0)
			for !C.atomic_compare_exchange_weak_u16(&ch.read_sub_mtx, &null16, u16(1)) {
				null16 = u16(0)
			}
			subscr[i].prev = unsafe { &ch.read_subscriber }
			unsafe {
				subscr[i].nxt = &Subscription(C.atomic_exchange_ptr(&voidptr(&ch.read_subscriber),
					&subscr[i]))
			}
			if voidptr(subscr[i].nxt) != voidptr(0) {
				subscr[i].nxt.prev = unsafe { &subscr[i].nxt }
			}
			C.atomic_store_u16(&ch.read_sub_mtx, u16(0))
		}
	}
	stopwatch := if timeout == time.infinite || timeout <= 0 {
		time.StopWatch{}
	} else {
		time.new_stopwatch()
	}
	mut event_idx := -1 // negative index means `timed out`

	outer: for {
		rnd := rand.intn(channels.len) or { 0 }
		mut num_closed := 0
		for j, _ in channels {
			mut i := j + rnd
			if i >= channels.len {
				i -= channels.len
			}
			if dir[i] == .push {
				stat := channels[i].try_push_priv(objrefs[i], true)
				if stat == .success {
					event_idx = i
					break outer
				} else if stat == .closed {
					num_closed++
				}
			} else {
				stat := channels[i].try_pop_priv(objrefs[i], true)
				if stat == .success {
					event_idx = i
					break outer
				} else if stat == .closed {
					num_closed++
				}
			}
		}
		if num_closed == channels.len {
			event_idx = -2
			break outer
		}
		if timeout <= 0 {
			break outer
		}
		if timeout != time.infinite {
			remaining := timeout - stopwatch.elapsed()
			if !sem.timed_wait(remaining) {
				break outer
			}
		} else {
			sem.wait()
		}
	}
	// reset subscribers
	for i, ch in channels {
		if dir[i] == .push {
			mut null16 := u16(0)
			for !C.atomic_compare_exchange_weak_u16(&ch.write_sub_mtx, &null16, u16(1)) {
				null16 = u16(0)
			}
			unsafe {
				*subscr[i].prev = subscr[i].nxt
			}
			if unsafe { subscr[i].nxt != 0 } {
				subscr[i].nxt.prev = subscr[i].prev
				// just in case we have missed a semaphore during restore
				subscr[i].nxt.sem.post()
			}
			C.atomic_store_u16(&ch.write_sub_mtx, u16(0))
		} else {
			mut null16 := u16(0)
			for !C.atomic_compare_exchange_weak_u16(&ch.read_sub_mtx, &null16, u16(1)) {
				null16 = u16(0)
			}
			unsafe {
				*subscr[i].prev = subscr[i].nxt
			}
			if unsafe { subscr[i].nxt != 0 } {
				subscr[i].nxt.prev = subscr[i].prev
				subscr[i].nxt.sem.post()
			}
			C.atomic_store_u16(&ch.read_sub_mtx, u16(0))
		}
	}
	sem.destroy()
	return event_idx
}
