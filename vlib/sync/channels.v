module sync

#include <stdatomic.h>

// the following functions are actually generic in C
fn C.atomic_load(voidptr) u64
fn C.atomic_compare_exchange_weak(voidptr, voidptr, u64) bool
fn C.atomic_compare_exchange_strong(voidptr, voidptr, u64) bool
fn C.atomic_fetch_add(voidptr, u64) u64
fn C.atomic_fetch_sub(voidptr, u64) u64

struct Channel {
	writesem Semaphore // to wake thread that wanted to write, but buffer was full
	readsem Semaphore // to wake thread that wanted to read, but buffer was empty
	ringbuf byteptr
	objsize u32
	buflen u64 // in number of objects
	bufsize u32 // in bytes
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
	write_subscriber Semaphore
	read_subscriber Semaphore
}

/*
pub fn new_channel<T>(n int) &Channel {
	return new_channel_raw(n, sizeof(T))
}
*/

pub fn new_channel<T>(n u32) &Channel {
	bufsize := n * sizeof(T)
	buf := if n > 0 { malloc(int(bufsize)) } else { byteptr(0) }
	return &Channel{
		writesem: new_semaphore()
		readsem:  new_semaphore()
		objsize: sizeof(T)
		buflen: n
		ringbuf: buf
		bufsize: bufsize
		buf_elem_write_ptr: voidptr(buf)
		buf_elem_read_ptr: voidptr(buf)
		write_free: C.atomic_ullong(n)
		write_subscriber: Semaphore{
			sem: 0
		}
		read_subscriber: Semaphore{
			sem: 0
		}
	}
}

pub fn (mut ch Channel) push(src voidptr) {
	mut wradr := C.atomic_load(&ch.write_adr)
	for {
		for wradr != C.NULL {
			thewradr := wradr
			mut nulladr := voidptr(0)
			if C.atomic_compare_exchange_strong(&ch.write_adr, &wradr, nulladr) {
				// there is a reader waiting for us
				C.memcpy(thewradr, src, ch.objsize)
				for !C.atomic_compare_exchange_weak(&ch.adr_written, &nulladr, thewradr) {
					nulladr = voidptr(0)
				}
				ch.readsem.post()
				return
			}
		}
		if ch.bufsize == 0 {
			// try to provide current object as readable
			mut nulladdr := voidptr(0)
			if C.atomic_compare_exchange_strong(&ch.read_adr, &nulladdr, src) {
				wradr = C.atomic_load(&ch.write_adr)
				if wradr != C.NULL {
					mut src2 := src
					if C.atomic_compare_exchange_strong(&ch.read_adr, &src2, voidptr(0)) {
						continue
					}
				}
				for {
					mut src2 := src
					ch.writesem.wait()
					if C.atomic_compare_exchange_strong(&ch.adr_read, &src2, voidptr(0)) {
						break
					} else {
						// this semaphore was not for us - repost in
						ch.writesem.post()
					}
				}
				return
			}
		} else {
			wr_free := C.atomic_load(&ch.write_free)
			if wr_free > 0 {
				if !C.atomic_compare_exchange_strong(&ch.write_free, &wr_free, wr_free-1) {
					continue // someone else has pushed inbetween - restart from beginning
				}
				mut wr_ptr := C.atomic_load(&ch.buf_elem_write_ptr)
				for {
					mut new_wr_ptr := wr_ptr + ch.objsize
					unsafe {
						if new_wr_ptr >= ch.ringbuf + ch.bufsize {
							new_wr_ptr -= ch.bufsize
						}
					}
					if C.atomic_compare_exchange_strong(&ch.buf_elem_write_ptr, &wr_ptr, new_wr_ptr) {
						break
					}
				}
				C.memcpy(wr_ptr, src, ch.objsize)
				C.atomic_fetch_add(&ch.read_avail, 1)
				mut rd_waiting := C.atomic_load(&ch.readers_waiting)
				for {
					if rd_waiting > 0 {
						if C.atomic_compare_exchange_strong(&ch.readers_waiting, &rd_waiting, rd_waiting - 1) {
							ch.readsem.post()
							break
						}
					} else {
						break
					}
				}
				return
			}
		}
		/*
		mut rd_waiting := C.atomic_load(&ch.readers_waiting)
		for {
			if rd_waiting > 0 {
				if C.atomic_compare_exchange_strong(&ch.readers_waiting, &rd_waiting, rd_waiting - 1) {
					ch.readsem.post()
					break
				}
			} else {
				break
			}
		}
		C.atomic_fetch_add(&ch.writers_waiting, 1)
		*/
		//ch.readsem.post()
		//ch.writesem.wait()
		// println('writer waiting')
		wradr = C.atomic_load(&ch.write_adr)
	}
}

pub fn (mut ch Channel) pop(dest voidptr) {
	mut rdadr := C.atomic_load(&ch.read_adr)
	for {
		for rdadr != C.NULL {
			therdadr := rdadr
			if C.atomic_compare_exchange_strong(&ch.read_adr, &rdadr, voidptr(0)) {
				// there is a writer waiting for us
				C.memcpy(dest, therdadr, ch.objsize)
				mut nulladr := voidptr(0)
				for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, therdadr) {
					nulladr = voidptr(0)
				}
				ch.writesem.post()
				return
			}
		}
		if ch.bufsize == 0 {
			// try to provide current object as writable
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
					ch.readsem.wait()
					if C.atomic_compare_exchange_strong(&ch.adr_written, &dest2, voidptr(0)) {
						break
					} else {
						// this semaphore was not for us - repost in
						ch.readsem.post()
					}
				}
				return
			}
		} else {
			rd_avail := C.atomic_load(&ch.read_avail)
			if rd_avail > 0 {
				if !C.atomic_compare_exchange_strong(&ch.read_avail, &rd_avail, rd_avail - 1) {
					continue // someone else has poped inbetween - restart from beginning
				}
				mut rd_ptr := C.atomic_load(&ch.buf_elem_read_ptr)
				for {
					mut new_rd_ptr := rd_ptr + ch.objsize
					unsafe {
						if new_rd_ptr >= ch.ringbuf + ch.bufsize {
							new_rd_ptr -= ch.bufsize
						}
					}
					if C.atomic_compare_exchange_strong(&ch.buf_elem_read_ptr, &rd_ptr, new_rd_ptr) {
						break
					}
				}
				C.memcpy(dest, rd_ptr, ch.objsize)
				C.atomic_fetch_add(&ch.write_free, 1)
				mut wr_waiting := C.atomic_load(&ch.writers_waiting)
				for {
					if wr_waiting > 0 {
						if C.atomic_compare_exchange_strong(&ch.writers_waiting, &wr_waiting, wr_waiting - 1) {
							ch.writesem.post()
							break
						}
					} else {
						break
					}
				}
				return
			}
		}
		/*
		mut wr_waiting := C.atomic_load(&ch.writers_waiting)
		for {
			if wr_waiting > 0 {
				if C.atomic_compare_exchange_strong(&ch.writers_waiting, &wr_waiting, wr_waiting - 1) {
					ch.writesem.post()
					break
				}
			} else {
				break
			}
		}
		C.atomic_fetch_add(&ch.readers_waiting, 1)
		*/
		//ch.writesem.post()
		//ch.readsem.wait()
		rdadr = C.atomic_load(&ch.read_adr)
	}
}
