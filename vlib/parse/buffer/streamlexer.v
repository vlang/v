module buffer

// import (
// 	"io"
// )

struct Block  {
	buf    []byte
	next   int // index in pool plus one
	active bool
}

struct BufferPool  {
	pool []Block
	head int // index in pool plus one
	tail int // index in pool plus one

	pos int // byte pos in tail
}

fn (z &BufferPool) swap(oldBuf []byte, size int) []byte {
	// find new buffer that can be reused
	swap := -1
	for i := 0; i < z.pool.len; i++ {
		if !z.pool[i].active && size <= cap(z.pool[i].buf) {
			swap = i
			break
		}
	}
	if swap == -1 { // no free buffer found for reuse
		// if z.tail == 0 && z.pos >= oldBuf.len && size <= cap(oldBuf) { // but we can reuse the current buffer!
		if z.tail == 0 && z.pos >= oldBuf.len { // but we can reuse the current buffer!
			z.pos -= oldBuf.len
			return oldBuf[:0]
		}
		// allocate new
		// z.pool = append(z.pool, Block{make([]byte, 0, size), 0, true})
		z.pool << Block{[]byte, 0, true}
		swap = z.pool.len - 1
	}

	newBuf := z.pool[swap].buf

	// put current buffer into pool
	z.pool[swap] = Block{oldBuf, 0, true}
	if z.head != 0 {
		z.pool[z.head-1].next = swap + 1
	}
	z.head = swap + 1
	if z.tail == 0 {
		z.tail = swap + 1
	}

	return newBuf[:0]
}

fn (z &BufferPool) free(n int) {
	z.pos += n
	// move the tail over to next buffers
	for z.tail != 0 && z.pos >= z.pool[z.tail-1].buf.len {
		z.pos -= z.pool[z.tail-1].buf.len
		newTail := z.pool[z.tail-1].next
		z.pool[z.tail-1].active = false // after this, any thread may pick up the inactive buffer, so it can't be used anymore
		z.tail = newTail
	}
	if z.tail == 0 {
		z.head = 0
	}
}

// StreamLexer is a buffered reader that allows peeking forward and shifting, taking an io.Reader.
// It keeps data in-memory until free, taking a byte length, is called to move beyond the data.
struct StreamLexer {
	r   io.Reader
	err error

	pool BufferPool

	buf       []byte
	start     int // index in buf
	pos       int // index in buf
	prevStart int

	free int
}

// new_stream_lexer returns a new StreamLexer for a given io.Reader with a 4kB estimated buffer size.
// If the io.Reader implements Bytes, that buffer is used instead.
fn new_stream_lexer(r io.Reader) *StreamLexer {
	return new_stream_lexer_size(r, defaultBufSize)
}

// new_stream_lexer_size returns a new StreamLexer for a given io.Reader and estimated required buffer size.
// If the io.Reader implements Bytes, that buffer is used instead.
fn new_stream_lexer_size(r io.Reader, size int) *StreamLexer {
	// if reader has the bytes in memory already, use that instead
	if buffer, ok := r.(interface {
		Bytes() []byte
	}); ok {
		return &StreamLexer{
			err: io.EOF,
			buf: buffer.Bytes(),
		}
	}
	return &StreamLexer{
		r:   r,
		buf: make([]byte, 0, size),
	}
}

fn (z &StreamLexer) read(pos int) byte {
	if z.err != nil {
		return 0
	}

	// free unused bytes
	z.pool.free(z.free)
	z.free = 0

	// get new buffer
	c := cap(z.buf)
	p := pos - z.start + 1
	if 2*p > c { // if the token is larger than half the buffer, increase buffer size
		c = 2*c + p
	}
	d := z.buf.len - z.start
	buf := z.pool.swap(z.buf[:z.start], c)
	copy(buf[:d], z.buf[z.start:]) // copy the left-overs (unfinished token) from the old buffer

	// read in new data for the rest of the buffer
	var n int
	for pos-z.start >= d && z.err == nil {
		n, z.err = z.r.Read(buf[d:cap(buf)])
		d += n
	}
	pos -= z.start
	z.pos -= z.start
	z.start, z.buf = 0, buf[:d]
	if pos >= d {
		return 0
	}
	return z.buf[pos]
}

// err returns the error returned from io.Reader. It may still return valid bytes for a while though.
fn (z &StreamLexer) err() error {
	if z.err == io.EOF && z.pos < z.buf.len {
		return nil
	}
	return z.err
}

// free frees up bytes of length n from previously shifted tokens.
// Each call to shift should at one point be followed by a call to free with a length returned by shift_len.
fn (z &StreamLexer) free(n int) {
	z.free += n
}

// peek returns the ith byte relative to the end position and possibly does an allocation.
// peek returns zero when an error has occurred, err returns the error.
// TODO: inline function
fn (z &StreamLexer) peek(pos int) byte {
	pos += z.pos
	if uint(pos) < uint(z.buf.len) { // uint for BCE
		return z.buf[pos]
	}
	return z.read(pos)
}

// peek_rune returns the rune and rune length of the ith byte relative to the end position.
fn (z &StreamLexer) peek_rune(pos int) (rune, int) {
	// from unicode/utf8
	c := z.peek(pos)
	if c < 0xC0 {
		return rune(c), 1
	} else if c < 0xE0 {
		return rune(c&0x1F)<<6 | rune(z.peek(pos+1)&0x3F), 2
	} else if c < 0xF0 {
		return rune(c&0x0F)<<12 | rune(z.peek(pos+1)&0x3F)<<6 | rune(z.peek(pos+2)&0x3F), 3
	}
	return rune(c&0x07)<<18 | rune(z.peek(pos+1)&0x3F)<<12 | rune(z.peek(pos+2)&0x3F)<<6 | rune(z.peek(pos+3)&0x3F), 4
}

// move advances the position.
fn (z &StreamLexer) move(n int) {
	z.pos += n
}

// pos returns a mark to which can be rewinded.
fn (z &StreamLexer) pos() int {
	return z.pos - z.start
}

// rewind rewinds the position to the given position.
fn (z &StreamLexer) rewind(pos int) {
	z.pos = z.start + pos
}

// lexeme returns the bytes of the current selection.
fn (z &StreamLexer) lexeme() []byte {
	return z.buf[z.start:z.pos]
}

// skip collapses the position to the end of the selection.
fn (z &StreamLexer) skip() {
	z.start = z.pos
}

// shift returns the bytes of the current selection and collapses the position to the end of the selection.
// It also returns the number of bytes we moved since the last call to shift. This can be used in calls to free.
fn (z &StreamLexer) shift() []byte {
	if z.pos > z.buf.len { // make sure we peeked at least as much as we shift
		z.read(z.pos - 1)
	}
	b := z.buf[z.start:z.pos]
	z.start = z.pos
	return b
}

// shift_len returns the number of bytes moved since the last call to shift_len. This can be used in calls to free because it takes into account multiple shifts or skips.
fn (z &StreamLexer) shift_len() int {
	n := z.start - z.prevStart
	z.prevStart = z.start
	return n
}
