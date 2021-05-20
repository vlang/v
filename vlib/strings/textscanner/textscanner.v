module textscanner

// TextScanner simplifies writing small scanners/parsers
// by providing safe methods to scan texts character by
// character, peek for the next characters, go back, etc.
pub struct TextScanner {
pub:
	input string
	ilen  int
mut:
	pos int // current position; pos is *always* kept in [0,ilen]
}

pub fn new(input string) TextScanner {
	return TextScanner{
		input: input
		ilen: input.len
	}
}

[unsafe]
pub fn (mut ss TextScanner) free() {
	unsafe {
		ss.input.free()
	}
}

// remaining - return how many characters remain in the input
[inline]
pub fn (ss &TextScanner) remaining() int {
	return ss.ilen - ss.pos
}

// next - safely get a character from the input text
[direct_array_access; inline]
pub fn (mut ss TextScanner) next() int {
	if ss.pos < ss.ilen {
		opos := ss.pos
		ss.pos++
		return ss.input[opos]
	}
	return -1
}

// skip - skip one character; skip() is slightly faster than .next()
// and ignoring the result.
[inline]
pub fn (mut ss TextScanner) skip() {
	if ss.pos + 1 < ss.ilen {
		ss.pos++
	}
}

// skip_n - skip the next `n` characters
[inline]
pub fn (mut ss TextScanner) skip_n(n int) {
	ss.pos += n
	if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
}

// peek - safely get the *next* character from the input text
// if the character exists. NB: unlike next(), peek() *will not* change
// the state of the scanner.
[direct_array_access; inline]
pub fn (ss &TextScanner) peek() int {
	if ss.pos < ss.ilen {
		return ss.input[ss.pos]
	}
	return -1
}

// peek_n - safely get the *next* character from the input text at the current
// position + `n`, if the character exists, or else it returns -1.
// NB: .peek() and .peek_offset(0) are equivalent.
[direct_array_access; inline]
pub fn (ss &TextScanner) peek_n(n int) int {
	if ss.pos + n < ss.ilen {
		return ss.input[ss.pos + n]
	}
	return -1
}

// back - go back a character
[inline]
pub fn (mut ss TextScanner) back() {
	if ss.pos > 0 {
		ss.pos--
	}
}

// back_n - go back `n` characters
pub fn (mut ss TextScanner) back_n(n int) {
	ss.pos -= n
	if ss.pos < 0 {
		ss.pos = 0
	}
	if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
}

// reset - go back to the start of the input
pub fn (mut ss TextScanner) reset() {
	ss.pos = 0
}
