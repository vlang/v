module textscanner

// TextScanner simplifies writing small scanners/parsers
// by providing safe methods to scan texts character by
// character, peek for the next characters, go back, etc.
// TODO: maybe a generic type is more suitable for this.
pub struct TextScanner {
pub:
	input_runes []rune
	input_bytes []u8
	ilen        int
pub mut:
	pos    int // current position; pos is *always* kept in [0,ilen]
	config TextScannerConfig
}

@[params]
pub struct TextScannerConfig {
pub mut:
	force_rune_mode bool
}

// new returns a stack allocated instance of TextScanner.
pub fn new(input string, config TextScannerConfig) TextScanner {
	if config.force_rune_mode {
		input_runes := input.runes()
		return TextScanner{
			input_runes: input_runes
			ilen:        input_runes.len
			config:      config
		}
	}
	input_bytes := input.bytes()
	return TextScanner{
		input_bytes: input_bytes
		ilen:        input_bytes.len
		config:      config
	}
}

// free frees all allocated resources.
@[unsafe]
pub fn (mut ss TextScanner) free() {
	unsafe {
		if ss.input_runes.len > 0 {
			ss.input_runes.free()
		}
		if ss.input_bytes.len > 0 {
			ss.input_bytes.free()
		}
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
		if ss.config.force_rune_mode {
			return int(ss.input_runes[opos])
		} else {
			return ss.input_bytes[opos]
		}
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
		if ss.config.force_rune_mode {
			return int(ss.input_runes[ss.pos])
		} else {
			return ss.input_bytes[ss.pos]
		}
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
		if ss.config.force_rune_mode {
			return u8(ss.input_runes[ss.pos])
		} else {
			return ss.input_bytes[ss.pos]
		}
	}
	return 0
}

// peek_n returns the character code from the input text at position + `n`.
// peek_n returns `-1` if it can't peek `n` characters ahead.
// ts.peek_n(0) == ts.current() .
// ts.peek_n(1) == ts.peek() .
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_n(n int) int {
	new_pos := ss.pos + n
	if new_pos < 0 || new_pos >= ss.ilen {
		return -1
	}
	if ss.config.force_rune_mode {
		return int(ss.input_runes[ss.pos + n])
	} else {
		return ss.input_bytes[ss.pos + n]
	}
}

// peek_n_u8 returns the character code from the input text, at position + `n`,
// as a byte/u8.
// Note: peek_n_u8 returns `0`, if it can't peek the next character.
// Note: use `peek_n()`, instead of `peek_n_u8()`, if your input itself can
// legitimately contain bytes with value `0`.
@[direct_array_access; inline]
pub fn (ss &TextScanner) peek_n_u8(n int) u8 {
	new_pos := ss.pos + n
	if new_pos < 0 || new_pos >= ss.ilen {
		return 0
	}
	if ss.config.force_rune_mode {
		return u8(ss.input_runes[ss.pos + n])
	} else {
		return ss.input_bytes[ss.pos + n]
	}
}

// back goes back one character from the current scanner position.
@[inline]
pub fn (mut ss TextScanner) back() {
	if ss.pos > 0 {
		ss.pos--
	}
}

// back_n goes back `n` characters from the current scanner position.
@[inline]
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
		if ss.config.force_rune_mode {
			return int(ss.input_runes[ss.pos - offset])
		} else {
			return ss.input_bytes[ss.pos - offset]
		}
	}
	return -1
}

// current returns the current character code from the input text.
// current returns `-1` at the start of the input text.
// Note: after `c := ts.next()`, `ts.current()` will also return `c`.
@[direct_array_access; inline]
pub fn (mut ss TextScanner) current() int {
	if ss.pos > 0 {
		if ss.config.force_rune_mode {
			return int(ss.input_runes[ss.pos - 1])
		} else {
			return ss.input_bytes[ss.pos - 1]
		}
	}
	return -1
}

// reset resets the internal state of the scanner
// After calling .reset(), .next() will start reading
// again from the start of the input text.
@[inline]
pub fn (mut ss TextScanner) reset() {
	ss.pos = 0
}

// goto_end has the same effect as `for ts.next() != -1 {}`
// i.e. after calling .goto_end(), the scanner will be at
// the end of the input text. Further .next() calls will
// return -1, unless you go back.
@[inline]
pub fn (mut ss TextScanner) goto_end() {
	ss.pos = ss.ilen
}

// skip_whitespace advances the scanner pass any space characters in the input.
@[inline]
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
	if ss.config.force_rune_mode {
		for i in start .. ss.ilen {
			if ss.input_runes[i] == `\r` || ss.input_runes[i] == `\n` {
				end = i
				break
			}
		}
		if ss.input_runes[end] == `\r` {
			// check next char is `\n`
			if end + 1 < ss.ilen && ss.input_runes[end + 1] == `\n` {
				ss.pos = end + 2
			} else {
				ss.pos = end + 1
			}
		} else {
			ss.pos = end + 1
		}
	} else {
		for i in start .. ss.ilen {
			if ss.input_bytes[i] == `\r` || ss.input_bytes[i] == `\n` {
				end = i
				break
			}
		}
		if ss.input_bytes[end] == `\r` {
			// check next char is `\n`
			if end + 1 < ss.ilen && ss.input_bytes[end + 1] == `\n` {
				ss.pos = end + 2
			} else {
				ss.pos = end + 1
			}
		} else {
			ss.pos = end + 1
		}
	}

	if end >= ss.ilen {
		ss.pos = ss.ilen
		if ss.config.force_rune_mode {
			return ss.input_runes[start..].string(), false
		} else {
			return ss.input_bytes[start..].bytestr(), false
		}
	}
	if ss.pos > ss.ilen {
		ss.pos = ss.ilen
	}
	if ss.config.force_rune_mode {
		return ss.input_runes[start..end].string(), true
	} else {
		return ss.input_bytes[start..end].bytestr(), true
	}
}

// read_until reads characters from the current scanning position
// until a delimiter (from the provided string `delimiters`) is encountered.
// The returned string includes all characters from the starting
// position up to (but ​not​ including) the first encountered
// delimiter. The scanner's position is advanced to the character
// immediately after the delimiter (or to the end of the input if
// no delimiter is found).
@[direct_array_access]
pub fn (mut ss TextScanner) read_until(delimiters string) !string {
	if delimiters.len == 0 {
		return error('delimiters cannot be empty')
	}
	if ss.pos >= ss.ilen {
		return error('already at EOF')
	}
	start := ss.pos
	mut current_pos := ss.pos
	if ss.config.force_rune_mode {
		delimiters_runes := delimiters.runes()
		for {
			if current_pos >= ss.ilen {
				break
			}
			r := ss.input_runes[current_pos]
			if r in delimiters_runes {
				end := current_pos
				ss.pos = end + 1
				return ss.input_runes[start..end].string()
			}
			current_pos += 1
		}
		ss.pos = ss.ilen
		return ss.input_runes[start..].string()
	} else {
		delimiters_bytes := delimiters.bytes()
		for {
			if current_pos >= ss.ilen {
				break
			}
			r := ss.input_bytes[current_pos]
			if r in delimiters_bytes {
				end := current_pos
				ss.pos = end + 1
				return ss.input_bytes[start..end].bytestr()
			}
			current_pos += 1
		}
		ss.pos = ss.ilen
		return ss.input_bytes[start..].bytestr()
	}
}

// substr return a sub string of input string from start to end.
pub fn (mut ss TextScanner) substr(start int, end int) string {
	if start < 0 || start > ss.ilen || end < 0 || end > ss.ilen || start >= end {
		return ''
	}
	if ss.config.force_rune_mode {
		return ss.input_runes[start..end].string()
	} else {
		return ss.input_bytes[start..end].bytestr()
	}
}
