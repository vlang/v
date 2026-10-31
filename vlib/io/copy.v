module io

// copy_bytes copies bytes from src into dst and returns the number copied.
// It never grows dst; the caller must ensure dst has enough capacity.
pub fn copy_bytes(mut dst []u8, src []u8) int {
	mut n := src.len
	if n > dst.len {
		n = dst.len
	}
	if n > 0 {
		unsafe { C.memcpy(dst.data, src.data, n) }
	}
	return n
}

// copy copies from src to dst until EOF or error. It returns the number of
// bytes copied.
pub fn copy(mut dst Writer, mut src Reader) !i64 {
	return copy_buffer(mut dst, mut src, mut []u8{len: 32 * 1024})
}

// copy_n copies n bytes from src to dst.
pub fn copy_n(mut dst Writer, mut src Reader, n i64) !i64 {
	mut buf := []u8{len: 32 * 1024}
	mut written := i64(0)
	mut remaining := n
	for remaining > 0 {
		to_read := if remaining < i64(buf.len) { int(remaining) } else { buf.len }
		nr := src.read(mut buf[..to_read]) or {
			if err is Eof {
				if written == n {
					return written
				}
				return error('io: unexpected EOF after ${written} of ${n} bytes')
			}
			return err
		}
		if nr == 0 {
			if written == n {
				return written
			}
			return error('io: unexpected EOF after ${written} of ${n} bytes')
		}
		mut nw := 0
		for nw < nr {
			wn := dst.write(buf[nw..nr]) or { return err }
			if wn <= 0 {
				return error('io: write returned ${wn}')
			}
			nw += wn
		}
		written += i64(nr)
		remaining -= i64(nr)
	}
	return written
}

// copy_buffer copies from src to dst using the provided buffer.
pub fn copy_buffer(mut dst Writer, mut src Reader, mut buf []u8) !i64 {
	if buf.len == 0 {
		buf = []u8{len: 32 * 1024}
	}
	mut written := i64(0)
	for {
		nr := src.read(mut buf) or {
			if err is Eof {
				break
			}
			return err
		}
		if nr == 0 {
			break
		}
		mut nw := 0
		for nw < nr {
			wn := dst.write(buf[nw..nr]) or { return err }
			if wn <= 0 {
				return error('io: write returned ${wn}')
			}
			nw += wn
		}
		written += i64(nr)
	}
	return written
}
