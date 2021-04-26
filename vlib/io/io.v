module io

const (
	buf_max_len = 5 * 1024
)

pub fn cp(dst Writer, src Reader) ? {
	mut buf := read_all(reader: src) or {
		return err
	}
	dst.write(buf) or {
		return
	}
	unsafe {
		buf.free()
	}
}
