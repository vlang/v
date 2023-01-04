module io

const (
	buf_max_len = 1024
)

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
