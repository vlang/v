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
	spinloops_sem = 500
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
	writesem_im         Semaphore
	readsem_im         Semaphore
	ringbuf            byteptr // queue for buffered channels
	statusbuf          byteptr // flags to synchronize write/read in ringbuf
	objsize            u32
	queue_length       u32 // in #objects
mut: // atomic
	write_adr          voidptr // if != NULL the next obj can be written here without wait
	read_adr           voidptr // if != NULL an obj can be read from here without wait
	adr_read           voidptr // used to identify origin of writesem
	adr_written        voidptr // used to identify origin of readsem
	write_free         u32 // for queue state
	read_avail         u32
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
		writesem: new_semaphore_init(if n > 0 { n + 1 } else { 1 })
		readsem:  new_semaphore_init(if n > 0 { u32(0) } else { 1 })
		writesem_im: new_semaphore()
		readsem_im: new_semaphore()
		objsize: objsize
		queue_length: n
		write_free: n
		read_avail: 0
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
	// unbuffered channel
	mut have_swapped := false
	for {
		mut got_sem := false
		mut wradr := C.atomic_load(&ch.write_adr)
		for wradr != C.NULL {
			if C.atomic_compare_exchange_strong(&ch.write_adr, &wradr, voidptr(0)) {
				// there is a reader waiting for us
				unsafe { C.memcpy(wradr, src, ch.objsize) }
				mut nulladr := voidptr(0)
				for !C.atomic_compare_exchange_weak(&ch.adr_written, &nulladr, wradr) {
					nulladr = voidptr(0)
				}
				ch.readsem_im.post()
				return true
			}
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
				return false
			}
			ch.writesem.wait()
		}
		if ch.queue_length == 0 {
			// try to advertise current object as readable
			mut read_in_progress := false
			mut nulladdr := voidptr(0)
			if C.atomic_compare_exchange_strong(&ch.read_adr, &nulladdr, src) {
				wradr = C.atomic_load(&ch.write_adr)
				if wradr != C.NULL {
					mut src2 := src
					if C.atomic_compare_exchange_strong(&ch.read_adr, &src2, voidptr(0)) {
						ch.writesem.post()
						continue
					} else {
						read_in_progress = true
					}
				}
			}
			if !read_in_progress {
				mut sem := Semaphore{} // for select
				sem.sem = C.atomic_load(&ch.read_subscriber.sem)
				if sem.sem != 0 {
					sem.post()
				}
			}
			mut src2 := src
			// Try to spin wait for src to be read
			for sp := u32(0); sp < spinloops_ || read_in_progress; sp++ {
				if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
					have_swapped = true
					// read_in_progress = true
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
				if have_swapped || C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
					ch.writesem.post()
					break
				} else {
					// this semaphore was not for us - repost in
					ch.writesem_im.post()
					src2 = src
				}
			}
			return true
		} else {
			// buffered channel
			mut space_in_queue := false
			mut wr_free := C.atomic_load_u32(&ch.write_free)
			for wr_free > 0 {
				space_in_queue = C.atomic_compare_exchange_weak_u32(&ch.write_free, &wr_free, wr_free-1)
				if space_in_queue {
					break
				}
			}
			if space_in_queue {
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
				mut expected_status := byte(int(BufferElemStat.unused))
				for {
					if C.atomic_compare_exchange_weak_byte(status_adr, &expected_status, byte(int(BufferElemStat.writing))) {
						break
					}
					expected_status = byte(int(BufferElemStat.unused))
				}
				unsafe {
					C.memcpy(wr_ptr, src, ch.objsize)
				}
				C.atomic_store_byte(status_adr, byte(int(BufferElemStat.written)))
				old_read_avail := C.atomic_fetch_add_u32(&ch.read_avail, 1)
				ch.readsem.post()
				return true
			} else {
				ch.writesem.post()
			}
		}
	}
}

pub fn (mut ch Channel) pop(dest voidptr) {
	ch.try_pop(dest, false)
}

fn (mut ch Channel) try_pop(dest voidptr, no_block bool) bool {
	spinloops_, spinloops_sem_ := if no_block { 1, 1 } else { spinloops, spinloops_sem }
	mut have_swapped := false
	mut write_in_progress := false
	for {
		mut got_sem := false
		if ch.queue_length == 0 {
			// unbuffered channel - first see if a `push()` has adversized
			mut rdadr := C.atomic_load(&ch.read_adr)
			for rdadr != C.NULL {
				if C.atomic_compare_exchange_strong(&ch.read_adr, &rdadr, voidptr(0)) {
					// there is a writer waiting for us
					unsafe { C.memcpy(dest, rdadr, ch.objsize) }
					mut nulladr := voidptr(0)
					for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, rdadr) {
						nulladr = voidptr(0)
					}
					ch.writesem_im.post()
					return true
				}
			}
			if no_block {
				return false
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
				return false
			}
			ch.readsem.wait()
		}
		if ch.queue_length > 0 {
			// try to get buffer token
			mut obj_in_queue := false
			mut rd_avail := C.atomic_load_u32(&ch.read_avail)
			for rd_avail > 0 {
				obj_in_queue = C.atomic_compare_exchange_weak_u32(&ch.read_avail, &rd_avail, rd_avail-1)
				if obj_in_queue {
					break
				}
			}
			if obj_in_queue {
				mut rd_idx := C.atomic_load_u32(&ch.buf_elem_read_idx)
				for {
					mut new_rd_idx := rd_idx + 1
					for new_rd_idx >= ch.queue_length {
						new_rd_idx -= ch.queue_length
					}
					if C.atomic_compare_exchange_weak_u32(&ch.buf_elem_read_idx, &rd_idx, new_rd_idx) {
						break
					}
				}
				mut rd_ptr := ch.ringbuf
				mut status_adr := ch.statusbuf
				unsafe {
					rd_ptr += rd_idx * ch.objsize
					status_adr += rd_idx
				}
				mut expected_status := byte(int(BufferElemStat.written))
				for {
					if C.atomic_compare_exchange_weak_byte(status_adr, &expected_status, byte(int(BufferElemStat.reading))) {
						break
					}
					expected_status = byte(int(BufferElemStat.written))
				}
				unsafe {
					C.memcpy(dest, rd_ptr, ch.objsize)
				}
				C.atomic_store_byte(status_adr, byte(int(BufferElemStat.unused)))
				old_write_free := C.atomic_fetch_add_u32(&ch.write_free, 1)
				ch.writesem.post()
				if old_write_free == 0 {
					mut sem := Semaphore{}
					sem.sem = C.atomic_load(&ch.write_subscriber.sem)
					if sem.sem != 0 {
						sem.post()
					}
				}
				return true
			}
		}
		// try to advertise `dest` as writable
		mut nulladdr := voidptr(0)
		if C.atomic_compare_exchange_strong(&ch.write_adr, &nulladdr, dest) {
			if ch.queue_length == 0 {
				mut rdadr := C.atomic_load(&ch.read_adr)
				if rdadr != C.NULL {
					mut dest2 := dest
					if C.atomic_compare_exchange_strong(&ch.write_adr, &dest2, voidptr(0)) {
						ch.readsem.post()
						continue
					} else {
						write_in_progress = true
					}
				}
			}
		}
		if ch.queue_length == 0 && !write_in_progress {
			mut sem := Semaphore{}
			sem.sem = C.atomic_load(&ch.write_subscriber.sem)
			if sem.sem != 0 {
				sem.post()
			}
		}
		mut dest2 := dest
		for sp := u32(0); sp < spinloops_ || write_in_progress; sp++ {
			if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
				have_swapped = true
				break
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
			if have_swapped || C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
				ch.readsem.post()
				break
			} else {
				// this semaphore was not for us - repost in
				ch.readsem_im.post()
				dest2 = dest
			}
		}
		return true
	}
}

pub fn channel_select(mut channels []&Channel, is_push []bool, mut objrefs []voidptr, timeout time.Duration) int {
	assert channels.len == is_push.len
	assert is_push.len == objrefs.len
	mut sem := new_semaphore()
	for i, ch in channels {
		oldsem := if is_push[i] {
			C.atomic_exchange(&ch.write_subscriber.sem, sem.sem)
		} else {
			C.atomic_exchange(&ch.read_subscriber.sem, sem.sem)
		}
		if oldsem != 0 {
			panic('channel_select: channel $i is already used in another `select`')
		}
	}
	stopwatch := if timeout == 0 { time.StopWatch{} } else { time.new_stopwatch({}) }
	mut event_idx := -1 // negative index means `timed out`
	for sem.try_wait() {}
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
			remaining := timeout - stopwatch.elapsed()
			if !sem.timed_wait(remaining) {
				goto restore
			}
		} else {
			sem.wait()
		}
	}
restore:
	// reset subscribers
	for i, ch in channels {
		if is_push[i] {
			C.atomic_exchange(&ch.write_subscriber.sem, voidptr(0))
		} else {
			C.atomic_exchange(&ch.read_subscriber.sem, voidptr(0))
		}
	}
	sem.destroy()
	return event_idx
}
