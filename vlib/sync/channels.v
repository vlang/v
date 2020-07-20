module sync

#include <stdatomic.h>

// the following functions are actually generic in C
fn C.atomic_load(voidptr) u64
fn C.atomic_store(voidptr, u64) u64
fn C.atomic_compare_exchange_weak(voidptr, voidptr, u64) bool
fn C.atomic_compare_exchange_strong(voidptr, voidptr, u64) bool
fn C.atomic_exchange(voidptr, u64) u64
fn C.atomic_fetch_add(voidptr, u64) u64
fn C.atomic_fetch_sub(voidptr, u64) u64

const (
	spinloops = 70 // How often to try to get data before semaphore is used to wait
	alignsize = 8 // 64 Bit
)

struct Channel {
	writesem Semaphore // to wake thread that wanted to write, but buffer was full
	readsem Semaphore // to wake thread that wanted to read, but buffer was empty
	ringbuf byteptr
	objsize u32
	objsize_full u32 // aligned, including cleanbit
	bufsize u32 // in bytes
	flagoffset u32
mut: // atomic
	write_free C.atomic_ullong
	read_avail C.atomic_ullong
	writers_waiting C.atomic_ullong
	readers_waiting C.atomic_ullong
	write_adr C.atomic_uintptr_t // if != NULL the next obj can be written here without wait
	read_adr C.atomic_uintptr_t // if != NULL an obj can be read from here without wait
	adr_read C.atomic_uintptr_t
	adr_written C.atomic_uintptr_t
	buf_elem_write_ptr C.atomic_uintptr_t
	buf_elem_read_ptr C.atomic_uintptr_t
	// for select
	mtx C.pthread_mutex_t
	write_subscriber Semaphore
	read_subscriber Semaphore
}

/*
pub fn new_channel<T>(n int) &Channel {
	return new_channel_raw(n, sizeof(T))
}
*/

pub fn new_channel<T>(n u32, is_ref bool) &Channel {
	objsize := if is_ref { sizeof(voidptr) } else { sizeof(T) }
	objsize_full := if is_ref { alignsize } else { alignsize * ((objsize + alignsize - 1) / alignsize) }
	bufsize := n * objsize_full
	buf := if n > 0 { malloc(int(bufsize)) } else { byteptr(0) }
	flagoffset := objsize_full - alignsize
	for i in 0 .. n {
		unsafe {
			adr := buf + flagoffset + u32(i) * objsize_full
			C.atomic_store(&u64(adr), 0x8000000000000000)
		}
	}
	mut ch := &Channel{
		writesem: new_semaphore()
		readsem:  new_semaphore()
		objsize: objsize
		objsize_full: objsize_full
		flagoffset: flagoffset
		ringbuf: voidptr(buf)
		bufsize: bufsize
		buf_elem_write_ptr: voidptr(buf)
		buf_elem_read_ptr: voidptr(buf)
		write_free: C.atomic_ullong(n)
		read_avail: C.atomic_ullong(0)
		write_subscriber: Semaphore{
			sem: 0
		}
		read_subscriber: Semaphore{
			sem: 0
		}
	}
	return ch
}

pub fn (mut ch Channel) push(src voidptr) {
	mut src_flag := src
	mut srclastword := u64(0)
	if ch.flagoffset == 0 {
		srclastword = *&u64(src)
	} else {
		unsafe {
			src_flag += ch.flagoffset
		}
		if ch.objsize & 0x07 != 0 {
			C.memcpy(&srclastword, src_flag, ch.objsize & 0x07)
		}
	}
	mut wradr := if ch.bufsize == 0 { C.atomic_load(&ch.write_adr) } else { u64(0) }
	for {
		if ch.bufsize == 0 {
			for wradr != C.NULL {
				mut nulladr := voidptr(0)
				if C.atomic_compare_exchange_strong(&ch.write_adr, &wradr, nulladr) {
					// there is a reader waiting for us
					C.memcpy(wradr, src, ch.objsize)
					for !C.atomic_compare_exchange_weak(&ch.adr_written, &nulladr, wradr) {
						nulladr = voidptr(0)
					}
					ch.readsem.post()
					return
				}
			}
		}
		mut wr_free := if ch.bufsize == 0 { 0 } else { C.atomic_load(&ch.write_free) }
		if wr_free == ch.bufsize { // synchronous channel or empty buffer
			// try to provide current object as readable
			mut nulladdr := voidptr(0)
			if C.atomic_compare_exchange_strong(&ch.read_adr, &nulladdr, src) {
				if ch.bufsize == 0 {
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
					for _ in 0 .. spinloops {
						if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
							have_swapped = true
							break
						}
						src2 = src
					}
					ch.writesem.wait()
					if have_swapped {
						return
					}
					if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
						break
					} else {
						// this semaphore was not for us - repost in
						ch.writesem.post()
					}
				}
				return
			}
		} else { // buffered channel
			for _ in 0 .. spinloops { // do some spinning
				for wr_free > 0 {
					if C.atomic_compare_exchange_strong(&ch.write_free, &wr_free, wr_free-1) {
						break
					}
				}
				if wr_free > 0 {
					break
				} else {
					wr_free = C.atomic_load(&ch.write_free)
				}
			}
			if wr_free > 0 {
				mut wr_ptr := C.atomic_load(&ch.buf_elem_write_ptr)
				for {
					mut new_wr_ptr := wr_ptr + ch.objsize_full
					unsafe {
						for new_wr_ptr >= ch.ringbuf + ch.bufsize {
							new_wr_ptr -= ch.bufsize
						}
					}
					if C.atomic_compare_exchange_weak(&ch.buf_elem_write_ptr, &wr_ptr, new_wr_ptr) {
						break
					}
				}
				mut placeholder := u64(0x8000000000000000)
				mut wr_ptr_last_word := wr_ptr
				unsafe {
					wr_ptr_last_word += ch.flagoffset
				}
				for {
					if C.atomic_compare_exchange_weak(&u64(wr_ptr_last_word), &placeholder, srclastword) {
						break
					}
					placeholder = 0x8000000000000000
				}
				if ch.flagoffset > 0 {
					C.memcpy(wr_ptr, src, ch.objsize_full - alignsize)
				}
				C.atomic_fetch_add(&ch.read_avail, 1)
				mut rd_waiting := C.atomic_load(&ch.readers_waiting)
				for rd_waiting > 0 {
					if C.atomic_compare_exchange_strong(&ch.readers_waiting, &rd_waiting, rd_waiting - 1) {
						ch.readsem.post()
						return
					}
				}
				return
			} else { // we must wait until there is space free in the buffer
				C.atomic_fetch_add(&ch.writers_waiting, 1)
				for {
					mut src2 := u64(-1)
					ch.writesem.wait()
					if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
						break
					} else {
						// this semaphore was not for us - repost in
						ch.writesem.post()
					}
				}
			}
		}
		wradr = if ch.bufsize == 0 { C.atomic_load(&ch.write_adr) } else { u64(0) }
	}
}

pub fn (mut ch Channel) pop(dest voidptr) {
	mut rdadr := C.atomic_load(&ch.read_adr)
	for {
		for rdadr != C.NULL {
			if C.atomic_compare_exchange_strong(&ch.read_adr, &rdadr, voidptr(0)) {
				// there is a writer waiting for us
				C.memcpy(dest, rdadr, ch.objsize)
				mut nulladr := voidptr(0)
				for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, rdadr) {
					nulladr = voidptr(0)
				}
				ch.writesem.post()
				return
			}
		}
		if ch.bufsize == 0 { // try to advertise `dest` as writable
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
					for _ in 0 .. spinloops {
						if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
							have_swapped = true
							break
						}
						dest2 = dest
					}
					ch.readsem.wait()
					if have_swapped {
						return
					}
					if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
						break
					} else {
						// this semaphore was not for us - repost in
						ch.readsem.post()
					}
				}
				return
			}
		} else { // try to read element from buffer
			mut rd_avail := C.atomic_load(&ch.read_avail)
			for _ in 0 .. spinloops {
				for rd_avail > 0 {
					if C.atomic_compare_exchange_strong(&ch.read_avail, &rd_avail, rd_avail - 1) {
						break
					}
				}
				if rd_avail > 0 {
					break
				} else {
					rd_avail = C.atomic_load(&ch.read_avail)
				}
			}
			if rd_avail > 0 {
				mut rd_ptr := C.atomic_load(&ch.buf_elem_read_ptr)
				for {
					mut new_rd_ptr := rd_ptr + ch.objsize_full
					unsafe {
						for new_rd_ptr >= ch.ringbuf + ch.bufsize {
							new_rd_ptr -= ch.bufsize
						}
					}
					if C.atomic_compare_exchange_weak(&ch.buf_elem_read_ptr, &rd_ptr, new_rd_ptr) {
						break
					}
				}
				mut rd_ptr_last_word := rd_ptr
				unsafe {
					rd_ptr_last_word += ch.flagoffset
				}
				mut x := u64(0)
				for {
					x = C.atomic_load(&u64(rd_ptr_last_word))
					if (x & 0x8000000000000000) == 0 {
						break
					}
				}
				// C.atomic_store(&u64(dest), x)
				C.memcpy(dest, rd_ptr, ch.objsize)
				C.atomic_store(&u64(rd_ptr_last_word), 0x8000000000000000)
				C.atomic_fetch_add(&ch.write_free, 1)
				mut wr_waiting := C.atomic_load(&ch.writers_waiting)
				for wr_waiting > 0 {
					if C.atomic_compare_exchange_weak(&ch.writers_waiting, &wr_waiting, wr_waiting - 1) {
						mut nulladr := voidptr(0)
						for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, u64(-1)) {
							nulladr = voidptr(0)
						}
						ch.writesem.post()
						break
					}
				}
				return
			} else {
				C.atomic_fetch_add(&ch.readers_waiting, 1)
				ch.readsem.wait()
			}
		}
		rdadr = C.atomic_load(&ch.read_adr)
	}
}
