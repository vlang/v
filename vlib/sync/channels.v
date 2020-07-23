module sync

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

const (
	spinloops = 70 // how often to try to get data before to wait for semaphore
)

struct Channel {
	writesem           Semaphore // to wake thread that wanted to write, but buffer was full
	readsem            Semaphore // to wake thread that wanted to read, but buffer was empty
	ringbuf            byteptr // queue for buffered channels
	statusbuf          byteptr // flags to synchronize write/read in ringbuf
	objsize            u32
	bufsize            u32 // in bytes
	nobjs              u32 // in #objects
mut: // atomic
	write_free         u32 // queue status
	read_avail         u32
	writers_waiting    u32
	readers_waiting    u32
	write_adr          voidptr // if != NULL the next obj can be written here without wait
	read_adr           voidptr // if != NULL an obj can be read from here without wait
	adr_read           voidptr // used to identify origin of writesem
	adr_written        voidptr // used to identify origin of readsem
	buf_elem_write_ptr voidptr
	buf_elem_read_ptr  voidptr
	// for select
	write_subscriber   Semaphore
	read_subscriber    Semaphore
}

pub fn new_channel<T>(n u32) &Channel {
	objsize := sizeof(T)
	bufsize := n * objsize
	buf := if n > 0 { malloc(int(bufsize)) } else { byteptr(0) }
	statusbuf := if n > 0 { vcalloc(int(n)) } else { byteptr(0) }
	mut ch := &Channel{
		writesem: new_semaphore()
		readsem:  new_semaphore()
		objsize: objsize
		nobjs: n
		ringbuf: voidptr(buf)
		statusbuf: voidptr(statusbuf)
		bufsize: bufsize
		buf_elem_write_ptr: voidptr(statusbuf)
		buf_elem_read_ptr: voidptr(statusbuf)
		write_free: n
	}
	return ch
}

pub fn (mut ch Channel) push(src voidptr) {
	mut wradr := if ch.bufsize == 0 { C.atomic_load(&ch.write_adr) } else { voidptr(0) }
	for {
		if ch.bufsize == 0 {
			for wradr != C.NULL {
				mut nulladr := voidptr(0)
				if C.atomic_compare_exchange_strong(&ch.write_adr, &wradr, nulladr) {
					// there is a reader waiting for us
					unsafe { C.memcpy(wradr, src, ch.objsize) }
					for !C.atomic_compare_exchange_weak(&ch.adr_written, &nulladr, wradr) {
						nulladr = voidptr(0)
					}
					ch.readsem.post()
					return
				}
			}
		}
		mut wr_free := if ch.bufsize == 0 { u32(0) } else { C.atomic_load_u32(&ch.write_free) }
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
					if C.atomic_compare_exchange_strong_u32(&ch.write_free, &wr_free, wr_free-1) {
						break
					}
				}
				if wr_free > 0 {
					break
				} else {
					wr_free = C.atomic_load_u32(&ch.write_free)
				}
			}
			if wr_free > 0 {
				mut wr_ptr := C.atomic_load(&ch.buf_elem_write_ptr)
				for {
					mut new_wr_ptr := wr_ptr
					unsafe {
						new_wr_ptr = new_wr_ptr + 1
						for new_wr_ptr >= ch.statusbuf + ch.nobjs {
							new_wr_ptr -= ch.nobjs
						}
					}
					if C.atomic_compare_exchange_weak(&ch.buf_elem_write_ptr, &wr_ptr, new_wr_ptr) {
						break
					}
				}
				for {
					if C.atomic_load_byte(&byte(wr_ptr)) == 0x00 {
						break
					}
				}
				unsafe { C.memcpy(ch.ringbuf + (u64(wr_ptr) - u64(ch.statusbuf)) * ch.objsize, src, ch.objsize) }
				C.atomic_store_byte(&byte(wr_ptr), byte(0x01))
				C.atomic_fetch_add_u32(&ch.read_avail, 1)
				mut rd_waiting := C.atomic_load_u32(&ch.readers_waiting)
				for rd_waiting > 0 {
					if C.atomic_compare_exchange_strong_u32(&ch.readers_waiting, &rd_waiting, rd_waiting - 1) {
						ch.readsem.post()
						return
					}
				}
				return
			} else { // we must wait until there is space free in the buffer
				C.atomic_fetch_add_u32(&ch.writers_waiting, 1)
				for {
					mut src2 := voidptr(-1)
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
		wradr = if ch.bufsize == 0 { C.atomic_load(&ch.write_adr) } else { voidptr(0) }
	}
}

pub fn (mut ch Channel) pop(dest voidptr) {
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
			mut rd_avail := C.atomic_load_u32(&ch.read_avail)
			for _ in 0 .. spinloops {
				for rd_avail > 0 {
					if C.atomic_compare_exchange_strong_u32(&ch.read_avail, &rd_avail, rd_avail - 1) {
						break
					}
				}
				if rd_avail > 0 {
					break
				} else {
					rd_avail = C.atomic_load_u32(&ch.read_avail)
				}
			}
			if rd_avail > 0 {
				mut rd_ptr := C.atomic_load(&ch.buf_elem_read_ptr)
				for {
					mut new_rd_ptr := rd_ptr
					unsafe {
						new_rd_ptr = new_rd_ptr + 1
						for new_rd_ptr >= ch.statusbuf + ch.nobjs {
							new_rd_ptr -= ch.nobjs
						}
					}
					if C.atomic_compare_exchange_weak(&ch.buf_elem_read_ptr, &rd_ptr, new_rd_ptr) {
						break
					}
				}
				mut x := byte(0)
				for {
					x = C.atomic_load_byte(&byte(rd_ptr))
					if x == 0x01 {
						break
					}
				}
				unsafe { C.memcpy(dest, ch.ringbuf + (u64(rd_ptr) - u64(ch.statusbuf)) * ch.objsize, ch.objsize) }
				C.atomic_store_byte(&byte(rd_ptr), byte(0x0))
				C.atomic_fetch_add_u32(&ch.write_free, 1)
				mut wr_waiting := C.atomic_load_u32(&ch.writers_waiting)
				for wr_waiting > 0 {
					if C.atomic_compare_exchange_weak_u32(&ch.writers_waiting, &wr_waiting, wr_waiting - 1) {
						mut nulladr := voidptr(0)
						for !C.atomic_compare_exchange_weak(&ch.adr_read, &nulladr, voidptr(-1)) {
							nulladr = voidptr(0)
						}
						ch.writesem.post()
						break
					}
				}
				return
			} else {
				C.atomic_fetch_add_u32(&ch.readers_waiting, 1)
				ch.readsem.wait()
			}
		}
		rdadr = C.atomic_load(&ch.read_adr)
	}
}
