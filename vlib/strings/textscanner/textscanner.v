module textscanner

// TextScanner simplifies writing small scanners/parsers
// by providing safe methods to scan texts character by
// character, peek for the next characters, go back, etc.
pub struct TextScanner {
pub:
	input string
	ilen  int
pub mut:
	pos int // current position; pos is *always* kept in [0,ilen]
}

// new returns a stack allocated instance of TextScanner.
pub fn new(input string) TextScanner {
	return TextScanner{
		input: input
		ilen:  input.len
	}
}

// free frees all allocated resources.
@[unsafe]
pub fn (mut ss TextScanner) free() {
	unsafe {
		ss.input.free()
	}
}

// remaining returns how many characters remain from current position.
@[inline]
pub fn (ss &TextScanner) remaining() int {
	return ss.ilen - ss.pos
}

// next returns the next character code from the input text.
// next returns `-1` if it can't reach the next character.
// next advances the scanner position.
@[direct_array_access; inline]
pub fn (mut ss TextScanner) next() int {
	if ss.pos < ss.ilen {
		opos := ss.pos
		ss.pos++
		return ss.input[opos]
	}
	return -1
}

// skip skips one character ahead; `skip()` is slightly faster than `.next()`.
// `skip()` does not return a result.
@[inline]
pub fn (mut ss TextScanner) skip() {
	if ss.pos < ss.ilen {
		ss.pos++
	}
}

// skip_n skips ahead `n` characters, stopping at the end of the input.
@[inline]
pub fn (mut ss TextScanner) skip_n(n int) {
	ss.pos += n
	if ss.pos < 0 {
		ss.pos = 0
	} else if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
}

// peek returns the *next* character code from the input text.
// peek returns `-1` if it can't peek the next character.
// unlike `next()`, `peek()` does not change the state of the scanner.
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek() int {
	if ss.pos < ss.ilen {
		return ss.input[ss.pos]
	}
	return -1
}

// peek_u8 returns the *next* character code from the input text, as a byte/u8.
// unlike `next()`, `peek_u8()` does not change the state of the scanner.
// Note: peek_u8 returns `0`, if it can't peek the next character.
// Note: use `peek()`, instead of `peek_u8()`, if your input itself can
// legitimately contain bytes with value `0`.
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_u8() u8 {
	if ss.pos < ss.ilen {
		return ss.input[ss.pos]
	}
	return 0
}

// peek_n returns the character code from the input text at position + `n`.
// peek_n returns `-1` if it can't peek `n` characters ahead.
// ts.peek_n(0) == ts.current() .
// ts.peek_n(1) == ts.peek() .
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_n(n int) int {
	if ss.pos + n < ss.ilen {
		return ss.input[ss.pos + n]
	}
	return -1
}

// peek_n_u8 returns the character code from the input text, at position + `n`,
// as a byte/u8.
// Note: peek_n_u8 returns `0`, if it can't peek the next character.
// Note: use `peek_n()`, instead of `peek_n_u8()`, if your input itself can
// legitimately contain bytes with value `0`.
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_n_u8(n int) u8 {
	if ss.pos + n < ss.ilen {
		return ss.input[ss.pos + n]
	}
	return 0
}

// back goes back one character from the current scanner position.
@[inline]
pub fn (mut ss TextScanner) back() {
	if ss.pos > 0 {
		ss.pos--
	}
}

// back_n goes back `n` characters from the current scanner position.
pub fn (mut ss TextScanner) back_n(n int) {
	ss.pos -= n
	if ss.pos < 0 {
		ss.pos = 0
	} else if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
}

// peek_back returns the *previous* character code from the input text.
// peek_back returns `-1` if it can't peek the previous character.
// unlike `back()`, `peek_back()` does not change the state of the scanner.
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_back() int {
	return ss.peek_back_n(1)
}

// peek_back_n returns the character code from the input text at position - `n`.
// peek_back_n returns `-1` if it can't peek `n` characters back.
// ts.peek_back_n(0) == ts.current()
// ts.peek_back_n(1) == ts.peek_back()
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_back_n(n int) int {
	offset := n + 1
	if ss.pos >= offset {
		return ss.input[ss.pos - offset]
	}
	return -1
}

// current returns the current character code from the input text.
// current returns `-1` at the start of the input text.
// Note: after `c := ts.next()`, `ts.current()` will also return `c`.
@[direct_array_access; inline]
pub fn (mut ss TextScanner) current() int {
	if ss.pos > 0 {
		return ss.input[ss.pos - 1]
	}
	return -1
}

// reset resets the internal state of the scanner
// After calling .reset(), .next() will start reading
// again from the start of the input text.
pub fn (mut ss TextScanner) reset() {
	ss.pos = 0
}

// goto_end has the same effect as `for ts.next() != -1 {}`
// i.e. after calling .goto_end(), the scanner will be at
// the end of the input text. Further .next() calls will
// return -1, unless you go back.
pub fn (mut ss TextScanner) goto_end() {
	ss.pos = ss.ilen
}

// skip_whitespace advances the scanner pass any space characters in the input.
pub fn (mut ss TextScanner) skip_whitespace() {
	for ss.ilen - ss.pos > 0 && ss.peek_u8().is_space() {
		ss.next()
	}
}

// next_line advances the scanner’s position to the start of
// the next line, and return the line.
// Returns true if successful, or false if the end of the input
// is reached.
@[direct_array_access]
pub fn (mut ss TextScanner) next_line() (string, bool) {
	if ss.pos == ss.ilen {
		return '', false
	}
	start := ss.pos
	mut end := ss.ilen
	for i in start .. ss.ilen {
		if ss.input[i] == `\n` || ss.input[i] == `\r` {
			end = i
			break
		}
	}

	if end >= ss.ilen {
		ss.pos = ss.ilen
		return ss.input[start..], false
	}
	if ss.input[end] == `\r` {
		// check next char is `\n`
		if end + 1 < ss.ilen && ss.input[end + 1] == `\n` {
			ss.pos = end + 2
		} else {
			ss.pos = end + 1
		}
	} else {
		ss.pos = end + 1
	}
	if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
	return ss.input[start..end], true
}

// read_until reads characters from the current scanning position
// until a delimiter (from the provided list `delimiters`) is encountered.
// The returned string includes all characters from the starting
// position up to (but ​not​ including) the first encountered
// delimiter. The scanner's position is advanced to the character
// immediately after the delimiter (or to the end of the input if
// no delimiter is found).
@[direct_array_access]
pub fn (mut ss TextScanner) read_until(delimiters []rune) !string {
	if delimiters.len == 0 {
		return error('delimiters cannot be empty')
	}
	if ss.pos >= ss.ilen {
		return error('already at EOF')
	}
	start := ss.pos
	mut current_pos := ss.pos
	for {
		if current_pos >= ss.ilen {
			break
		}
		r := ss.input[current_pos]
		if r in delimiters {
			end := current_pos
			ss.pos = end + 1
			return ss.input[start..end]
		}
		current_pos += 1
	}
	ss.pos = ss.ilen
	return ss.input[start..]
}
