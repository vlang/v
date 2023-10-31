module term

// utf8_getchar returns an utf8 rune from standard input
pub fn utf8_getchar() ?rune {
	c := getchar()
	if c == C.EOF {
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
			c2 := getchar()
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

// utf8_len calculates the length of a utf8 rune to read, according to its first byte
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
