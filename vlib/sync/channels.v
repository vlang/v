module sync

import time

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
	spinloops_sem = 120
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
	ch.timed_push(src, -1)
}

pub fn (mut ch Channel) timed_push(src voidptr, timeout time.Duration) bool {
	spinloops_, spinloops_sem_ := if i64(timeout) < 0 { spinloops, spinloops_sem } else { 1, 1 }
	if ch.queue_length > 0 { // buffered channel
		if i64(timeout) > 0 {
			if !ch.writesem.timed_wait(timeout) {
				return false
			}
		} else {
			mut got_sem := false
			for _ in 0 .. spinloops_sem_ {
				if ch.writesem.try_wait() {
					got_sem = true
					break
				}
				if i64(timeout) == 0 {
					return false
				}
			}
			if !got_sem {
				ch.writesem.wait()
			}
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
		// try to advertise current object as readable
		mut nulladdr := voidptr(0)
		if C.atomic_compare_exchange_strong(&ch.read_adr, &nulladdr, src) {
			if ch.queue_length == 0 {
				wradr = C.atomic_load(&ch.write_adr)
				if wradr != C.NULL {
					mut src2 := src
					if C.atomic_compare_exchange_strong(&ch.read_adr, &src2, voidptr(0)) {
						continue
					}
				}
			}
			for {
				mut src2 := src
				// Try to spin wait for src to be read
				mut have_swapped := false
				for _ in 0 .. spinloops_ {
					if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
						have_swapped = true
						break
					}
					src2 = src
				}
				if i64(timeout) > 0 {
					// TODO this has to be absolute time in multi receiver scenarios
					if !ch.writesem.timed_wait(timeout) {
						return false
					}
				} else {
					mut got_sem := false
					for _ in 0 .. spinloops_sem_ {
						if ch.writesem.try_wait() {
							got_sem = true
							break
							if i64(timeout) == 0 {
								return false
							}
						}
					}
					if !got_sem {
						ch.writesem.wait()
					}
				}
				if have_swapped {
					return true
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
	ch.timed_pop(dest, -1)
}

pub fn (mut ch Channel) timed_pop(dest voidptr, timeout time.Duration) bool {
	spinloops_, spinloops_sem_ := if i64(timeout) < 0 { spinloops, spinloops_sem } else { 1, 1 }
	if ch.queue_length > 0 { // buffered channel - try to read element from buffer
		if i64(timeout) > 0 {
			if !ch.readsem.timed_wait(timeout) {
				return false
			}
		} else {
			mut got_sem := false
			for _ in 0 .. spinloops_sem_ {
				if ch.readsem.try_wait() {
					got_sem = true
					break
				}
				if i64(timeout) == 0 {
					return false
				}
			}
			if !got_sem {
				ch.readsem.wait()
			}
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
		return true
	}
	// unbuffered channel
	mut rdadr := if ch.queue_length == 0 { C.atomic_load(&ch.read_adr) } else { voidptr(0) }
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
		// try to advertise `dest` as writable
		mut nulladdr := voidptr(0)
		if C.atomic_compare_exchange_strong(&ch.write_adr, &nulladdr, dest) {
			rdadr = C.atomic_load(&ch.read_adr)
			if rdadr != C.NULL {
				mut dest2 := dest
				if C.atomic_compare_exchange_strong(&ch.write_adr, &dest2, voidptr(0)) {
					continue
				}
			}
			for {
				mut dest2 := dest
				mut have_swapped := false
				for _ in 0 .. spinloops_ {
					if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
						have_swapped = true
						break
					}
					dest2 = dest
				}
				if i64(timeout) > 0 {
					if !ch.readsem.timed_wait(timeout) {
						return false
					}
				} else {
					mut got_sem := false
					for _ in 0 .. spinloops_sem_ {
						if ch.readsem.try_wait() {
							got_sem = true
							break
						}
						if i64(timeout) == 0 {
							return false
						}
					}
					if !got_sem {
						ch.readsem.wait()
					}
				}
				if have_swapped {
					return true
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
	}
	rdadr = C.atomic_load(&ch.read_adr)
}
