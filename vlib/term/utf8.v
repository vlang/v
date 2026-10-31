module term

// utf8_getchar returns an utf8 rune from standard input.
pub fn utf8_getchar() ?rune {
	c := input_character()
	if c == -1 {
		return none
	}
	len := utf8_len(u8(~c))
	if c < 0 {
		return 0
	} else if len == 0 {
		return c
	} else if len == 1 {
		return -1
	} else {
		mut uc := c & ((1 << (7 - len)) - 1)
		for i := 0; i + 1 < len; i++ {
			c2 := input_character()
			if c2 != -1 && (c2 >> 6) == 2 {
				uc <<= 6
				uc |= (c2 & 63)
			} else if c2 == -1 {
				return 0
			} else {
				return -1
			}
		}
		return uc
	}
}

// utf8_len calculates the length of a utf8 rune to read, according to its first byte.
pub fn utf8_len(c u8) int {
	mut b := 0
	mut x := c
	if (x & 240) != 0 {
		// 0xF0
		x >>= 4
	} else {
		b += 4
	}
	if (x & 12) != 0 {
		// 0x0C
		x >>= 2
	} else {
		b += 2
	}
	if (x & 2) == 0 {
		// 0x02
		b++
	}
	return b
}

// printable_len returns the grapheme width of `s` after removing ANSI escape sequences.
// It uses the same width rules as utf8_str_visible_length. Cursor movement and
// control characters are not simulated as terminal operations.
pub fn printable_len(s string) int {
	if !s.contains('\x1b') {
		return utf8_str_visible_length(s)
	}
	mut visible := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] == 0x1b {
			i = skip_ansi(s, i)
		} else {
			visible << s[i]
			i++
		}
	}
	return utf8_str_visible_length(visible.bytestr())
}

// skip_ansi returns the index just past the ANSI escape beginning at s[i].
@[inline]
fn skip_ansi(s string, i int) int {
	mut j := i + 1
	if j >= s.len {
		return j
	}
	match s[j] {
		`[` { // CSI
			j++
			for j < s.len {
				if s[j] >= 0x40 && s[j] <= 0x7e {
					return j + 1
				}
				j++
			}
			return j
		}
		`]` { // OSC, terminated by BEL or ST
			j++
			for j < s.len {
				if s[j] == 0x07 {
					return j + 1
				}
				if s[j] == 0x1b && j + 1 < s.len && s[j + 1] == `\\` {
					return j + 2
				}
				j++
			}
			return j
		}
		`P`, `X`, `^`, `_` { // DCS / SOS / PM / APC, terminated by ST
			j++
			for j < s.len {
				if s[j] == 0x1b && j + 1 < s.len && s[j + 1] == `\\` {
					return j + 2
				}
				j++
			}
			return j
		}
		else {
			// ESC may have intermediate bytes, as in the charset selection ESC ( B.
			for j < s.len && s[j] >= 0x20 && s[j] <= 0x2f {
				j++
			}
			if j < s.len && s[j] >= 0x30 && s[j] <= 0x7e {
				return j + 1
			}
			return j
		}
	}
}
