module sync

import time
import rand

#flag windows -I @VROOT/thirdparty/stdatomic/win
#flag linux -I @VROOT/thirdparty/stdatomic/nix
#flag darwin -I @VROOT/thirdparty/stdatomic/nix
#flag freebsd -I @VROOT/thirdparty/stdatomic/nix
#flag solaris -I @VROOT/thirdparty/stdatomic/nix

$if linux {
	$if tinyc {
		// most Linux distributions have /usr/lib/libatomic.so, but Ubuntu uses gcc version specific dir
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/8 -L/usr/lib/gcc/x86_64-linux-gnu/9 -latomic
	}
}

#include <atomic.h>

// the following functions are actually generic in C
fn C.atomic_load(voidptr) voidptr
fn C.atomic_store(voidptr, voidptr)
fn C.atomic_compare_exchange_weak(voidptr, voidptr, voidptr) bool
fn C.atomic_compare_exchange_strong(voidptr, voidptr, voidptr) bool
fn C.atomic_exchange(voidptr, voidptr) voidptr
fn C.atomic_fetch_add(voidptr, voidptr) voidptr
fn C.atomic_fetch_sub(voidptr, voidptr) voidptr

fn C.atomic_load_byte(voidptr) byte
fn C.atomic_store_byte(voidptr, byte)
fn C.atomic_compare_exchange_weak_byte(voidptr, voidptr, byte) bool
fn C.atomic_compare_exchange_strong_byte(voidptr, voidptr, byte) bool
fn C.atomic_exchange_byte(voidptr, byte) byte
fn C.atomic_fetch_add_byte(voidptr, byte) byte
fn C.atomic_fetch_sub_byte(voidptr, byte) byte

fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_compare_exchange_weak_u32(voidptr, voidptr, u32) bool
fn C.atomic_compare_exchange_strong_u32(voidptr, voidptr, u32) bool
fn C.atomic_exchange_u32(voidptr, u32) u32
fn C.atomic_fetch_add_u32(voidptr, u32) u32
fn C.atomic_fetch_sub_u32(voidptr, u32) u32

fn C.atomic_load_u64(voidptr) u64
fn C.atomic_store_u64(voidptr, u64)
fn C.atomic_compare_exchange_weak_u64(voidptr, voidptr, u64) bool
fn C.atomic_compare_exchange_strong_u64(voidptr, voidptr, u64) bool
fn C.atomic_exchange_u64(voidptr, u64) u64
fn C.atomic_fetch_add_u64(voidptr, u64) u64
fn C.atomic_fetch_sub_u64(voidptr, u64) u64

const (
	 // how often to try to get data without blocking before to wait for semaphore
	spinloops = 250
	spinloops_sem = 5
)

enum BufferElemStat {
	unused = 0
	writing
	written
	reading
}

struct Channel {
	writesem           Semaphore // to wake thread that wanted to write, but buffer was full
	readsem            Semaphore // to wake thread that wanted to read, but buffer was empty
	ringbuf            byteptr // queue for buffered channels
	statusbuf          byteptr // flags to synchronize write/read in ringbuf
	objsize            u32
	queue_length       u32 // in #objects
mut: // atomic
	write_adr          voidptr // if != NULL the next obj can be written here without wait
	read_adr           voidptr // if != NULL an obj can be read from here without wait
	adr_read           voidptr // used to identify origin of writesem
	adr_written        voidptr // used to identify origin of readsem
	buf_elem_write_idx u32
	buf_elem_read_idx  u32
	// for select
	write_subscriber   Semaphore
	read_subscriber    Semaphore
}

pub fn new_channel<T>(n u32) &Channel {
	objsize := sizeof(T)
	buf := if n > 0 { malloc(int(n * objsize)) } else { byteptr(0) }
	statusbuf := if n > 0 { vcalloc(int(n)) } else { byteptr(0) }
	mut ch := &Channel{
		writesem: new_semaphore_init(n)
		readsem:  new_semaphore()
		objsize: objsize
		queue_length: n
		ringbuf: buf
		statusbuf: statusbuf
	}
	return ch
}

pub fn (mut ch Channel) push(src voidptr) {
	ch.try_push(src, false)
}

fn (mut ch Channel) try_push(src voidptr, no_block bool) bool {
	spinloops_, spinloops_sem_ := if no_block { 1, 1 } else { spinloops, spinloops_sem }
	if ch.queue_length > 0 { // buffered channel
		mut got_sem := false
		for _ in 0 .. spinloops_sem_ {
			if ch.writesem.try_wait() {
				got_sem = true
				break
			}
			if no_block {
				return false
			}
		}
		if !got_sem {
			ch.writesem.wait()
		}
		mut wr_idx := C.atomic_load_u32(&ch.buf_elem_write_idx)
		for {
			mut new_wr_idx := wr_idx + 1
			for new_wr_idx >= ch.queue_length {
				new_wr_idx -= ch.queue_length
			}
			if C.atomic_compare_exchange_strong_u32(&ch.buf_elem_write_idx, &wr_idx, new_wr_idx) {
				break
			}
		}
		mut wr_ptr := ch.ringbuf
		mut status_adr := ch.statusbuf
		unsafe {
			wr_ptr += wr_idx * ch.objsize
			status_adr += wr_idx
		}
		mut expected_status := byte(BufferElemStat.unused)
		for {
			if C.atomic_compare_exchange_weak_byte(status_adr, &expected_status, byte(BufferElemStat.writing)) {
				break
			}
			expected_status = byte(BufferElemStat.unused)
		}
		unsafe {
			C.memcpy(wr_ptr, src, ch.objsize)
		}
		C.atomic_store_byte(status_adr, byte(BufferElemStat.written))
		ch.readsem.post()
		if ch.read_subscriber.sem != 0 {
			ch.read_subscriber.post()
		}
		return true
	}
	// unbuffered channel
	mut wradr := C.atomic_load(&ch.write_adr)
	for {
		for wradr != C.NULL {
			mut nulladr := voidptr(0)
			if C.atomic_compare_exchange_strong(&ch.write_adr, &wradr, nulladr) {
				// there is a reader waiting for us
				unsafe { C.memcpy(wradr, src, ch.objsize) }
				for !C.atomic_compare_exchange_weak(&ch.adr_written, &nulladr, wradr) {
					nulladr = voidptr(0)
				}
				ch.readsem.post()
				return true
			}
		}
		if no_block {
			return false
		}
		// try to advertise current object as readable
		mut nulladdr := voidptr(0)
		if C.atomic_compare_exchange_strong(&ch.read_adr, &nulladdr, src) {
			wradr = C.atomic_load(&ch.write_adr)
			mut read_in_progress := false
			if wradr != C.NULL {
				mut src2 := src
				if C.atomic_compare_exchange_strong(&ch.read_adr, &src2, voidptr(0)) {
					continue
				} else {
					read_in_progress = true
				}
			}
			if !read_in_progress {
				mut sem := Semaphore{} // for select
				sem.sem = C.atomic_load(&ch.read_subscriber.sem)
				if sem.sem != 0 {
					sem.post()
				}
			}
			for {
				mut src2 := src
				// Try to spin wait for src to be read
				mut have_swapped := false
				for sp := u32(0); sp < spinloops_ || read_in_progress; sp++ {
					if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
						have_swapped = true
						read_in_progress = true
						break
					}
					src2 = src
				}
				mut got_sem := false
				for sp := u32(0); sp < spinloops_ || read_in_progress; sp++ {
					if ch.writesem.try_wait() {
						got_sem = true
						break
					}
				}
				if !got_sem {
					ch.writesem.wait()
				}
				if have_swapped {
					break
				}
				if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
					break
				} else {
					// this semaphore was not for us - repost in
					ch.writesem.post()
				}
			}
			return true
		}
		wradr = C.atomic_load(&ch.write_adr)
	}
}

pub fn (mut ch Channel) pop(dest voidptr) {
	ch.try_pop(dest, false)
}

fn (mut ch Channel) try_pop(dest voidptr, no_block bool) bool {
	spinloops_, spinloops_sem_ := if no_block { 1, 1 } else { spinloops, spinloops_sem }
	if ch.queue_length > 0 { // buffered channel - try to read element from buffer
		mut got_sem := false
		for _ in 0 .. spinloops_sem_ {
			if ch.readsem.try_wait() {
				got_sem = true
				break
			}
			if no_block {
				return false
			}
		}
		if !got_sem {
			ch.readsem.wait()
		}
		mut rd_idx := C.atomic_load_u32(&ch.buf_elem_read_idx)
		for {
			mut new_rd_idx := rd_idx + 1
			for new_rd_idx >= ch.queue_length {
				new_rd_idx -= ch.queue_length
			}
			if C.atomic_compare_exchange_strong_u32(&ch.buf_elem_read_idx, &rd_idx, new_rd_idx) {
				break
			}
		}
		mut rd_ptr := ch.ringbuf
		mut status_adr := ch.statusbuf
		unsafe {
			rd_ptr += rd_idx * ch.objsize
			status_adr += rd_idx
		}
		mut expected_status := byte(BufferElemStat.written)
		for {
			if C.atomic_compare_exchange_weak_byte(status_adr, &expected_status, byte(BufferElemStat.reading)) {
				break
			}
			expected_status = byte(BufferElemStat.written)
		}
		unsafe {
			C.memcpy(dest, rd_ptr, ch.objsize)
		}
		C.atomic_store_byte(status_adr, byte(BufferElemStat.unused))
		ch.writesem.post()
		if ch.write_subscriber.sem != 0 {
			ch.write_subscriber.post()
		}
		return true
	}
	// unbuffered channel
	mut rdadr := C.atomic_load(&ch.read_adr)
	for {
		for rdadr != C.NULL {
			if C.atomic_compare_exchange_strong(&ch.read_adr, &rdadr, voidptr(0)) {
				// there is a writer waiting for us
				unsafe { C.memcpy(dest, rdadr, ch.objsize) }
				mut nulladr := voidptr(0)
				for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, rdadr) {
					nulladr = voidptr(0)
				}
				ch.writesem.post()
				return true
			}
		}
		if no_block {
			return false
		}
		// try to advertise `dest` as writable
		mut nulladdr := voidptr(0)
		if C.atomic_compare_exchange_strong(&ch.write_adr, &nulladdr, dest) {
			rdadr = C.atomic_load(&ch.read_adr)
			mut write_in_progress := false
			if rdadr != C.NULL {
				mut dest2 := dest
				if C.atomic_compare_exchange_strong(&ch.write_adr, &dest2, voidptr(0)) {
					continue
				} else {
					// we know that there is a write to `dest` in progress so no need to sem.wait()
					write_in_progress = true
				}
			}
			if !write_in_progress {
				// inform ongoing select about new data
				mut sem := Semaphore{}
				sem.sem = C.atomic_load(&ch.write_subscriber.sem)
				if sem.sem != 0 {
					sem.post()
				}
			}
			for {
				mut dest2 := dest
				mut have_swapped := false
				for sp := u32(0); sp < spinloops_ || write_in_progress; sp++ {
					if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
						have_swapped = true
						write_in_progress = true
						break
					}
					dest2 = dest
				}
				mut got_sem := false
				for sp := u32(0); sp < spinloops_sem_ || write_in_progress; sp++ {
					if ch.readsem.try_wait() {
						got_sem = true
						break
					}
				}
				if !got_sem {
					ch.readsem.wait()
				}
				if have_swapped {
					break
				}
				if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
					break
				} else {
					// this semaphore was not for us - repost in
					ch.readsem.post()
				}
			}
			return true
		}
		rdadr = C.atomic_load(&ch.read_adr)
	}
}

pub fn channel_select(mut channels []&Channel, is_push []bool, mut objrefs []voidptr, mut oldsems []voidptr, timeout time.Duration) int {
	assert channels.len == is_push.len
	assert is_push.len == objrefs.len
	assert objrefs.len == oldsems.len
	mut sem := new_semaphore()
	for i, ch in channels {
		nulladr := voidptr(0)
		if is_push[i] {
			C.atomic_compare_exchange_strong(&ch.write_subscriber.sem, &nulladr, sem.sem)
		} else {
			C.atomic_compare_exchange_strong(&ch.read_subscriber.sem, &nulladr, sem.sem)
		}
	}
	mut event_idx := -1 // negative value indicating timeout if not changed
	for {
		rnd := rand.u32_in_range(0, u32(channels.len))
		for j, _ in channels {
			mut i := j + int(rnd)
			if i >= channels.len {
				i -= channels.len
			}
			if is_push[i] {
				if channels[i].try_push(objrefs[i], true) {
					event_idx = i
					goto restore
				}
			} else {
				if channels[i].try_pop(objrefs[i], true) {
					event_idx = i
					goto restore
				}
			}
		}
		if timeout > 0 {
			if !sem.timed_wait(timeout) {
				goto restore
			}
		} else {
			sem.wait()
		}
	}
restore:
	// restore old subscribers
	for i, ch in channels {
		mut cmp_sem := sem.sem
		if is_push[i] {
			C.atomic_compare_exchange_strong(&ch.write_subscriber.sem, &cmp_sem, voidptr(0))
		} else {
			C.atomic_compare_exchange_strong(&ch.read_subscriber.sem, &cmp_sem, voidptr(0))
		}
	}
	return event_idx
}
