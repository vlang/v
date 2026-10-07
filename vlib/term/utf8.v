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

// printable_len returns the number of terminal columns `s` occupies when printed. ANSI escape sequences (CSI, OSC, DCS/APC/PM, and two-byte escapes) are skipped.
pub fn printable_len(s string) int {
	runes := s.runes()
	mut i := 0
	// Skip leading escapes so the first visible rune can seed state.
	for i < runes.len && runes[i] == `\x1b` {
		i = skip_ansi(runes, i)
	}
	if i >= runes.len {
		return 0
	}
	first_prop := grapheme_break_property(runes[i])
	mut state := grapheme_state_from_rune(runes[i], first_prop)
	mut total := 0
	mut cluster_width := utf8_rune_visible_width(runes[i], first_prop)
	i++
	for i < runes.len {
		if runes[i] == `\x1b` {
			i = skip_ansi(runes, i)
			continue
		}
		r := runes[i]
		prop := grapheme_break_property(r)
		if should_break_grapheme(state, r, prop) {
			total += cluster_width
			cluster_width = utf8_rune_visible_width(r, prop)
			state = grapheme_state_from_rune(r, prop)
			i++
			continue
		}
		rw := utf8_rune_visible_width(r, prop)
		if rw > cluster_width {
			cluster_width = rw
		}
		state.push(r, prop)
		i++
	}
	return total + cluster_width
}

// skip_ansi returns the index just past the ANSI escape sequence beginning at runes[i]
@[inline]
fn skip_ansi(runes []rune, i int) int {
	mut j := i + 1
	if j >= runes.len {
		return j
	}
	match runes[j] {
		`[` { // CSI
			j++
			for j < runes.len {
				c := runes[j]
				if c >= 0x40 && c <= 0x7e {
					return j + 1
				}
				j++
			}
			return j
		}
		`]` { // OSC
			j++
			for j < runes.len {
				c := runes[j]
				if c == 0x07 { // BEL
					return j + 1
				}
				if c == `\x1b` && j + 1 < runes.len && runes[j + 1] == `\\` {
					return j + 2 // ST
				}
				j++
			}
			return j
		}
		`P`, `X`, `^`, `_` { // DCS / APC / PM
			j++
			for j < runes.len {
				if runes[j] == `\x1b` && j + 1 < runes.len && runes[j + 1] == `\\` {
					return j + 2
				}
				j++
			}
			return j
		}
		else { // two-byte escape
			return j + 1
		}
	}
}
