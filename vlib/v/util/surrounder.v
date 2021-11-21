module util

import strings

[noinit]
pub struct Surrounder {
pub mut:
	befores []string
	afters  []string
}

pub fn new_surrounder(expected_length int) Surrounder {
	return Surrounder{
		befores: []string{cap: expected_length}
		afters: []string{cap: expected_length}
	}
}

pub fn (mut s Surrounder) add(before string, after string) {
	s.befores << before
	s.afters << after
}

[manualfree]
pub fn (s &Surrounder) before() string {
	len := s.befores.len
	if len > 0 {
		mut res := strings.new_builder(len * 100)
		defer {
			unsafe { res.free() }
		}
		for i := 0; i < len; i++ {
			x := &s.befores[i]
			if x.len > 0 {
				res.writeln(x)
			}
		}
		ret := res.str()
		return ret
	}
	return ''
}

[manualfree]
pub fn (s &Surrounder) after() string {
	len := s.afters.len
	if len > 0 {
		mut res := strings.new_builder(len * 100)
		defer {
			unsafe { res.free() }
		}
		for i := len - 1; i >= 0; i-- {
			x := &s.afters[i]
			if x.len > 0 {
				res.writeln(x)
			}
		}
		ret := res.str()
		return ret
	}
	return ''
}

pub fn (s &Surrounder) builder_write_befores(mut sb strings.Builder) {
	len := s.befores.len
	if len > 0 {
		for i := 0; i < len; i++ {
			x := &s.befores[i]
			if x.len > 0 {
				sb.writeln(x)
			}
		}
	}
}

pub fn (s &Surrounder) builder_write_afters(mut sb strings.Builder) {
	len := s.afters.len
	if len > 0 {
		for i := len - 1; i >= 0; i-- {
			x := &s.afters[i]
			if x.len > 0 {
				sb.writeln(x)
			}
		}
	}
}

[unsafe]
pub fn (mut s Surrounder) free() {
	unsafe {
		s.befores.free()
		s.afters.free()
	}
}
