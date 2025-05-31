module validate

struct Utf8State {
mut:
	index    int
	subindex int
	failed   bool
}

// utf8_string returns true, if the given string `s` consists only of valid UTF-8 runes
pub fn utf8_string(s string) bool {
	return utf8_data(s.str, s.len)
}

// utf8_data returns true, if the given `data` block, with length `len` bytes, consists only of valid UTF-8 runes
pub fn utf8_data(data &u8, len int) bool {
	mut state := Utf8State{}
	for i := 0; i < len; i++ {
		s := unsafe { data[i] }
		if s == 0 {
			break
		}
		state.next_state(s)
		if state.failed {
			return false
		}
	}
	return !state.failed && state.subindex <= 0
}

fn (mut s Utf8State) seq(r0 bool, r1 bool, is_tail bool) bool {
	if s.subindex == 0 || (s.index > 1 && s.subindex == 1) || (s.index >= 6 && s.subindex == 2) {
		if (s.subindex == 0 && r0) || (s.subindex == 1 && r1) || (s.subindex == 2 && is_tail) {
			s.subindex++
			return true
		}
	} else {
		s.failed = true
		if is_tail {
			s.index = 0
			s.subindex = 0
			s.failed = false
		}
		return true
	}
	s.index++
	s.subindex = 0
	return false
}

/* Check UTF-8 Byte sequences according to Unicode Standard
 * https://www.unicode.org/versions/Unicode16.0.0/core-spec/chapter-3/
 * Code Points        1st       2s       3s       4s
 * U+0000..U+007F     00..7F
 * U+0080..U+07FF     C2..DF   80..BF
 * U+0800..U+0FFF     E0       A0..BF   80..BF
 * U+1000..U+CFFF     E1..EC   80..BF   80..BF
 * U+D000..U+D7FF     ED       80..9F   80..BF
 * U+E000..U+FFFF     EE..EF   80..BF   80..BF
 * U+10000..U+3FFFF   F0       90..BF   80..BF   80..BF
 * U+40000..U+FFFFF   F1..F3   80..BF   80..BF   80..BF
 * U+100000..U+10FFFF F4       80..8F   80..BF   80..BF
 */
fn (mut s Utf8State) next_state(c u8) {
	// sequence 1
	if s.index == 0 {
		if (c >= 0x00 + 1 && c <= 0x7F) || c == 0x00 {
			return
		}
		s.index++
		s.subindex = 0
	}
	is_tail := c >= 0x80 && c <= 0xBF
	// sequence 2
	if s.index == 1 && s.seq(c >= 0xC2 && c <= 0xDF, false, is_tail) {
		return
	}
	// sequence 3
	if s.index == 2 && s.seq(c == 0xE0, c >= 0xA0 && c <= 0xBF, is_tail) {
		return
	}
	if s.index == 3 && s.seq(c >= 0xE1 && c <= 0xEC, c >= 0x80 && c <= 0xBF, is_tail) {
		return
	}
	if s.index == 4 && s.seq(c == 0xED, c >= 0x80 && c <= 0x9F, is_tail) {
		return
	}
	if s.index == 5 && s.seq(c >= 0xEE && c <= 0xEF, c >= 0x80 && c <= 0xBF, is_tail) {
		return
	}
	// sequence 4
	if s.index == 6 && s.seq(c == 0xF0, c >= 0x90 && c <= 0xBF, is_tail) {
		return
	}
	if s.index == 7 && s.seq(c >= 0xF1 && c <= 0xF3, c >= 0x80 && c <= 0xBF, is_tail) {
		return
	}
	if s.index == 8 && s.seq(c == 0xF4, c >= 0x80 && c <= 0x8F, is_tail) {
		return
	}
	// we should never reach here
	s.failed = true
}
