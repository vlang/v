module io

// copy_bytes copies bytes from src into dst and returns the number copied.
// It copies min(dst.len, src.len) bytes without growing dst. The slices may overlap.
pub fn copy_bytes(mut dst []u8, src []u8) int {
	mut n := src.len
	if n > dst.len {
		n = dst.len
	}
	if n > 0 {
		// Move bytes directly so overlapping slices retain their original source data.
		unsafe { vmemmove(dst.data, src.data, n) }
	}
	return n
}

// copy copies from src to dst until src reports Eof, returning the bytes copied.
// It uses a 32 KiB buffer and retries short writes. Other errors propagate.
// Invalid counts and zero-byte reads or writes return errors, possibly after partial output.
pub fn copy(mut dst Writer, mut src Reader) !i64 {
	return copy_buffer(mut dst, mut src, mut []u8{len: 32 * 1024})
}

// copy_n copies n bytes from src to dst.
// A nonpositive n succeeds without reading or writing. Early Eof is an error.
// Short writes are retried; invalid counts and zero-byte reads or writes are errors.
// Errors may leave partial output in dst.
pub fn copy_n(mut dst Writer, mut src Reader, n i64) !i64 {
	if n <= 0 {
		return 0
	}
	mut buf := []u8{len: 32 * 1024}
	mut written := i64(0)
	mut remaining := n
	for remaining > 0 {
		to_read := if remaining < i64(buf.len) { int(remaining) } else { buf.len }
		nr := src.read(mut buf[..to_read]) or {
			if err is Eof {
				return error('io: unexpected EOF after ${written} of ${n} bytes')
			}
			return err
		}
		validate_reader_count(nr, to_read)!
		if nr == 0 {
			return error('io: copy_n: no progress')
		}
		mut nw := 0
		for nw < nr {
			wn := dst.write(buf[nw..nr]) or { return err }
			if wn <= 0 || wn > nr - nw {
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
// An empty buffer is replaced with a 32 KiB buffer. Otherwise its length is unchanged.
// Reader Eof completes the copy; other errors propagate. Short writes are retried.
// Invalid counts and zero-byte reads or writes return errors, possibly after partial output.
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
		validate_reader_count(nr, buf.len)!
		if nr == 0 {
			return error('io: copy_buffer: no progress')
		}
		mut nw := 0
		for nw < nr {
			wn := dst.write(buf[nw..nr]) or { return err }
			if wn <= 0 || wn > nr - nw {
				return error('io: write returned ${wn}')
			}
			nw += wn
		}
		written += i64(nr)
	}
	return written
}
