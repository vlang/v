module sync

struct Channel {
	writesem Semaphore
	readsem Semaphore
	objsize u32
	bufsize u32
	ringbuf byteptr
mut: // atomic
	write_idx u32
	read_idx u32
	write_subscriber Semaphore
	read_subscriber Semaphore
}

/*
pub fn new_channel<T>(n int) &Channel {
	return new_channel_raw(n, sizeof(T))
}
*/

pub fn new_channel<T>(n int) &Channel {
	assert n > 0
	bufsize := u32(n) * sizeof(T)
	return &Channel{
		writesem: new_semaphore_init(n)
		readsem:  new_semaphore()
		objsize: sizeof(T)
		bufsize: bufsize
		ringbuf: malloc(int(bufsize))
		write_idx: 0
		read_idx: 0
		write_subscriber: Semaphore{
			sem: 0
		}
		read_subscriber: Semaphore{
			sem: 0
		}
	}
}

pub fn (mut ch Channel) push(src voidptr) {
	ch.writesem.wait()
	old_wr_idx := ch.write_idx
	ch.write_idx += ch.objsize
	if ch.write_idx >= ch.bufsize {
		ch.write_idx -= ch.bufsize
	}
	unsafe {
		C.memcpy(ch.ringbuf+old_wr_idx, src, ch.objsize)
	}
	ch.readsem.post()
}

pub fn (mut ch Channel) pop(dest voidptr) {
	ch.readsem.wait()
	old_rd_idx := ch.read_idx
	ch.read_idx += ch.objsize
	if ch.read_idx >= ch.bufsize {
		ch.read_idx -= ch.bufsize
	}
	unsafe {
		C.memcpy(dest, ch.ringbuf + old_rd_idx, ch.objsize)
	}
	ch.writesem.post()
}
