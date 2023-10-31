module io

const (
	buf_max_len = 1024
)

// cp copies from `src` to `dst` by allocating
// a maximum of 1024 bytes buffer for reading
// until either EOF is reached on `src` or an error occurs.
// An error is returned if an error is encountered during write.
pub fn cp(mut src Reader, mut dst Writer) ! {
	mut buf := []u8{len: io.buf_max_len}
	for {
		len := src.read(mut buf) or { break }
		dst.write(buf[..len]) or { return err }
	}
	unsafe {
		buf.free()
	}
}
