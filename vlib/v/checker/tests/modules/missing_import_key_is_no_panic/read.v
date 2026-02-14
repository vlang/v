module foo

pub fn (mut c Connection) read() !string {
	mut res := []u8{}
	mut nn := 0
	for {
		mut buf := []u8{len: c.buf_size, cap: c.buf_size}
		n := c.stream.read(mut buf) or {
			if err is io.Eof {
				break
			} else {
				return err
			}
		}
		nn += n
		if n == 0 {
			break
		}
		res << buf[..n]
		if res[nn - 1] == `\n` {
			break
		}
	}
	return res#[..-1].bytestr()
}
