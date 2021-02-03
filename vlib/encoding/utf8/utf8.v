module utf8

struct Utf8State {
mut:
	index    int
	subindex int
	failed   bool
}

pub fn validate_str(str string) bool {
	return validate(str.str, str.len)
}

pub fn validate(data byteptr, len int) bool {
	mut state := Utf8State{}
	for i := 0; i < len; i++ {
		s := unsafe {data[i]}
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

fn (mut s Utf8State) next_state(c byte) {
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
