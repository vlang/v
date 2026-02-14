module foo

import io

struct Connection {
	buf_size int = 4096
mut:
	stream io.ReaderWriter
}
