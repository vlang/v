module io

// Reader represents a stream of data that can be read 
pub interface Reader {
	read(mut buf []byte) ?int
}

// make_reader is a temp that converts a struct to a reader
// (e.g. for use in struct initialisation)
pub fn make_reader(r Reader) Reader {
	return r
}

const (
	read_all_len = 10 * 1024
	read_all_grow_len = 1024
)

// read_all reads all available bytes from a reader
pub fn read_all(r Reader) ?[]byte {
	mut b := []byte{len:read_all_len}
	mut read := 0
	for {
		read += r.read(mut b[read..]) or {
			break
		}
		if read == 0 || b.len != b.cap {
			break
		}
		b.grow(read_all_grow_len)
	}
	return b
}

// RandomReader represents a stream of data that can be read from at a random location
interface RandomReader {
	read_from(pos int, mut buf []byte) ?int
}
