package buffer

import (
	"io"
	"io/ioutil"
)

const (
	NullBuffer = [byte(0)]
)

// Lexer is a buffered reader that allows peeking forward and shifting, taking an io.Reader.
// It keeps data in-memory until Free, taking a byte length, is called to move beyond the data.
struct Lexer {
	buf   []byte
	pos   int // index in buf
	start int // index in buf
	err   error

	restore func()
}

// new_lexer_bytes returns a new Lexer for a given io.Reader, and uses ioutil.ReadAll to read it into a byte slice.
// If the io.Reader implements bytes, that is used instead.
// It will append a NULL at the end of the buffer.
fn new_lexer(r io.Reader) *Lexer {
	var b []byte
	if r != nil {
		if buffer, ok := r.(interface {
			bytes() []byte
		}); ok {
			b = buffer.bytes()
		} else {
			var err error
			b, err = ioutil.ReadAll(r)
			if err != nil {
				return &Lexer{
					buf: []byte{0},
					err: err,
				}
			}
		}
	}
	return new_lexer_bytes(b)
}

// new_lexer_bytes returns a new Lexer for a given byte slice, and appends NULL at the end.
// To avoid reallocation, make sure the capacity has room for one more byte.
fn new_lexer_bytes(b []byte) *Lexer {
	z := &Lexer{
		buf: b,
	}

	n := len(b)
	if n == 0 {
		z.buf = NullBuffer
	} else if b[n-1] != 0 {
		// Append NULL to buffer, but try to avoid reallocation
		if cap(b) > n {
			// Overwrite next byte but restore when done
			b = b[:n+1]
			c := b[n]
			b[n] = 0

			z.buf = b
			z.restore = func() {
				b[n] = c
			}
		} else {
			z.buf = append(b, 0)
		}
	}
	return z
}

// restore restores the replaced byte past the end of the buffer by NULL.
fn (z *Lexer) restore() {
	if z.restore != nil {
		z.restore()
		z.restore = nil
	}
}

// err returns the error returned from io.Reader or io.EOF when the end has been reached.
fn (z *Lexer) err() error {
	return z.peek_err(0)
}

// peek_err returns the error at position pos. When pos is zero, this is the same as calling err().
fn (z *Lexer) peek_err(pos int) error {
	if z.err != nil {
		return z.err
	} else if z.pos+pos >= len(z.buf)-1 {
		return io.EOF
	}
	return nil
}

// peek returns the ith byte relative to the end position.
// Peek returns 0 when an error has occurred, err returns the error.
fn (z *Lexer) peek(pos int) byte {
	pos += z.pos
	return z.buf[pos]
}

// peek_rune returns the rune and rune length of the ith byte relative to the end position.
fn (z *Lexer) peek_rune(pos int) (rune, int) {
	// from unicode/utf8
	c := z.peek(pos)
	if c < 0xC0 || z.peek(pos+1) == 0 {
		return rune(c), 1
	} else if c < 0xE0 || z.peek(pos+2) == 0 {
		return rune(c&0x1F)<<6 | rune(z.peek(pos+1)&0x3F), 2
	} else if c < 0xF0 || z.peek(pos+3) == 0 {
		return rune(c&0x0F)<<12 | rune(z.peek(pos+1)&0x3F)<<6 | rune(z.peek(pos+2)&0x3F), 3
	}
	return rune(c&0x07)<<18 | rune(z.peek(pos+1)&0x3F)<<12 | rune(z.peek(pos+2)&0x3F)<<6 | rune(z.peek(pos+3)&0x3F), 4
}

// move advances the position.
fn (z *Lexer) move(n int) {
	z.pos += n
}

// pos returns a mark to which can be rewinded.
fn (z *Lexer) pos() int {
	return z.pos - z.start
}

// rewind rewinds the position to the given position.
fn (z *Lexer) rewind(pos int) {
	z.pos = z.start + pos
}

// lexeme returns the bytes of the current selection.
fn (z *Lexer) lexeme() []byte {
	return z.buf[z.start:z.pos]
}

// skip collapses the position to the end of the selection.
fn (z *Lexer) skip() {
	z.start = z.pos
}

// shift returns the bytes of the current selection and collapses the position to the end of the selection.
fn (z *Lexer) shift() []byte {
	b := z.buf[z.start:z.pos]
	z.start = z.pos
	return b
}

// offset returns the character position in the buffer.
fn (z *Lexer) offset() int {
	return z.pos
}

// bytes returns the underlying buffer.
fn (z *Lexer) bytes() []byte {
	return z.buf[:len(z.buf)-1]
}
