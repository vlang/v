module strconv

// write_dec writes the decimal representation of `n` into `buf`.
// It returns the number of bytes written, or `-1` if `buf` is too small.
@[direct_array_access]
pub fn write_dec(n i64, mut buf []u8) int {
	mut mag := u64(n)
	if n < 0 {
		mag = u64(0) - mag
		ndigits := dec_digits(mag)
		if buf.len < ndigits + 1 {
			return -1
		}
		buf[0] = `-`
		write_dec_u_digits(mag, mut buf, 1, ndigits)
		return ndigits + 1
	}
	ndigits := dec_digits(mag)
	if buf.len < ndigits {
		return -1
	}
	write_dec_u_digits(mag, mut buf, 0, ndigits)
	return ndigits
}

// write_dec_u writes the decimal representation of `n` into `buf`.
// It returns the number of bytes written, or `-1` if `buf` is too small.
@[direct_array_access]
pub fn write_dec_u(n u64, mut buf []u8) int {
	ndigits := dec_digits(n)
	if buf.len < ndigits {
		return -1
	}
	write_dec_u_digits(n, mut buf, 0, ndigits)
	return ndigits
}

@[direct_array_access]
fn write_dec_u_digits(n u64, mut buf []u8, offset int, ndigits int) {
	mut x := n
	mut i := offset + ndigits
	for {
		i--
		buf[i] = u8(x % 10) + `0`
		x /= 10
		if x == 0 {
			break
		}
	}
}
