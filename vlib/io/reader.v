module io

// Reader represents a stream of data that can be read
pub interface Reader {
	// read reads up to buf.len bytes and places
	// them into buf.
	// A struct that implements this should return 
	// `none` on end of stream (EOF) instead of just returning 0
	read(mut buf []byte) ?int
}

// make_reader is a temp that converts a struct to a reader
// (e.g. for use in struct initialisation)
pub fn make_reader(r Reader) Reader {
	return r
}

pub const (
	eof_code = -1
	eof = error_with_code('EOF', eof_code)
)

const (
	read_all_len      = 10 * 1024
	read_all_grow_len = 1024
)

// read_all reads all available bytes from a reader
pub fn read_all(r Reader) ?[]byte {
	mut b := []byte{len:read_all_len}
	mut read := 0
	for {
		new_read := r.read(mut b[read..]) or {
			break
		}
		read += new_read
		if new_read == 0 {
			break
		}
		if b.len == read {
			b.grow(read_all_grow_len)
		}
	}
	return b[..read]
}

// RandomReader represents a stream of data that can be read from at a random location
interface RandomReader {
	read_from(pos int, mut buf []byte) ?int
}
