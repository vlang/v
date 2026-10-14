import strings

// The truncating methods of strings.Builder clamp an out of range argument to the buffer.
// They should never read outside of it, and they should never leave it with a negative length.

struct CutCase {
	arg  int
	cut  string // what the call should return
	rest string // what should remain in the builder
}

struct PartCase {
	start int
	n     int
	res   string
}

fn new_abcde() strings.Builder {
	mut b := strings.new_builder(8)
	b.write_string('abcde')
	return b
}

// check_rest asserts that the builder contains `rest`, and that it is still usable.
fn check_rest(mut b strings.Builder, rest string) {
	assert b.len == rest.len
	assert b.after(0) == rest
	b.write_string('XY')
	assert b.len == rest.len + 2
	assert b.str() == rest + 'XY'
	assert b.len == 0
	b.write_string('again')
	assert b.str() == 'again'
}

fn test_go_back() {
	cases := [
		CutCase{0, '', 'abcde'},
		CutCase{1, '', 'abcd'},
		CutCase{4, '', 'a'},
		CutCase{5, '', ''}, // exactly the length of the buffer
		CutCase{6, '', ''}, // one past it
		CutCase{99, '', ''},
		CutCase{max_int, '', ''},
		CutCase{-1, '', 'abcde'},
		CutCase{-99, '', 'abcde'},
		CutCase{min_int, '', 'abcde'},
	]
	for c in cases {
		mut b := new_abcde()
		b.go_back(c.arg)
		assert b.len == c.rest.len, 'go_back(${c.arg})'
		check_rest(mut b, c.rest)
	}
}

fn test_cut_last() {
	cases := [
		CutCase{0, '', 'abcde'},
		CutCase{2, 'de', 'abc'},
		CutCase{4, 'bcde', 'a'},
		CutCase{5, 'abcde', ''}, // exactly the length of the buffer
		CutCase{6, 'abcde', ''}, // one past it
		CutCase{99, 'abcde', ''},
		CutCase{max_int, 'abcde', ''},
		CutCase{-1, '', 'abcde'},
		CutCase{-99, '', 'abcde'},
		CutCase{min_int, '', 'abcde'},
	]
	for c in cases {
		mut b := new_abcde()
		cut := b.cut_last(c.arg)
		assert cut.len == c.cut.len, 'cut_last(${c.arg})'
		assert cut == c.cut, 'cut_last(${c.arg})'
		assert b.len == c.rest.len, 'cut_last(${c.arg})'
		check_rest(mut b, c.rest)
	}
}

fn test_cut_to() {
	cases := [
		CutCase{0, 'abcde', ''},
		CutCase{3, 'de', 'abc'},
		CutCase{4, 'e', 'abcd'},
		CutCase{5, '', 'abcde'}, // exactly the length of the buffer
		CutCase{6, '', 'abcde'}, // one past it
		CutCase{99, '', 'abcde'},
		CutCase{max_int, '', 'abcde'},
		CutCase{-1, 'abcde', ''},
		CutCase{-99, 'abcde', ''},
		CutCase{min_int, 'abcde', ''},
	]
	for c in cases {
		mut b := new_abcde()
		cut := b.cut_to(c.arg)
		assert cut.len == c.cut.len, 'cut_to(${c.arg})'
		assert cut == c.cut, 'cut_to(${c.arg})'
		assert b.len == c.rest.len, 'cut_to(${c.arg})'
		check_rest(mut b, c.rest)
	}
}

fn test_go_back_to() {
	cases := [
		CutCase{0, '', ''},
		CutCase{3, '', 'abc'},
		CutCase{4, '', 'abcd'},
		CutCase{5, '', 'abcde'}, // exactly the length of the buffer
		CutCase{6, '', 'abcde'}, // one past it
		CutCase{99, '', 'abcde'},
		CutCase{max_int, '', 'abcde'},
		CutCase{-1, '', ''},
		CutCase{-99, '', ''},
		CutCase{min_int, '', ''},
	]
	for c in cases {
		mut b := new_abcde()
		b.go_back_to(c.arg)
		assert b.len == c.rest.len, 'go_back_to(${c.arg})'
		check_rest(mut b, c.rest)
	}
}

fn test_spart() {
	cases := [
		// in range
		PartCase{0, 5, 'abcde'},
		PartCase{0, 1, 'a'},
		PartCase{1, 2, 'bc'},
		PartCase{4, 1, 'e'},
		// empty ranges
		PartCase{0, 0, ''},
		PartCase{3, 0, ''},
		PartCase{5, 0, ''},
		// the range ends after the end of the buffer
		PartCase{0, 6, 'abcde'},
		PartCase{0, 99, 'abcde'},
		PartCase{4, 2, 'e'},
		PartCase{3, 5, 'de'},
		PartCase{1, max_int, 'bcde'},
		// the range starts at the end of the buffer, or after it
		PartCase{5, 1, ''},
		PartCase{6, 1, ''},
		PartCase{99, 1, ''},
		PartCase{max_int, 1, ''},
		PartCase{max_int, max_int, ''},
		// the range starts before the start of the buffer
		PartCase{-1, 2, 'a'},
		PartCase{-1, 1, ''},
		PartCase{-3, 99, 'abcde'},
		PartCase{-2, 4, 'ab'},
		PartCase{-99, 5, ''},
		PartCase{min_int, max_int, ''},
		PartCase{min_int, 5, ''},
		// negative counts
		PartCase{0, -1, ''},
		PartCase{2, -1, ''},
		PartCase{5, -1, ''},
		PartCase{-1, -1, ''},
		PartCase{0, min_int, ''},
		PartCase{-1, min_int, ''},
	]
	for c in cases {
		mut b := new_abcde()
		res := b.spart(c.start, c.n)
		assert res.len == c.res.len, 'spart(${c.start}, ${c.n})'
		assert res == c.res, 'spart(${c.start}, ${c.n})'
		// spart does not change the builder:
		check_rest(mut b, 'abcde')
	}
}

fn test_empty_builder() {
	mut b := strings.new_builder(0)
	for n in [-1, 0, 1, 99] {
		b.go_back(n)
		assert b.len == 0
		assert b.cut_last(n) == ''
		assert b.len == 0
		assert b.cut_to(n) == ''
		assert b.len == 0
		b.go_back_to(n)
		assert b.len == 0
		assert b.spart(n, 1) == ''
		assert b.spart(0, n) == ''
		assert b.len == 0
	}
	check_rest(mut b, '')
}

// last_n and after get their results from spart
fn test_last_n_and_after_with_a_negative_argument() {
	mut b := new_abcde()
	assert b.last_n(-1) == ''
	assert b.last_n(-99) == ''
	assert b.last_n(0) == ''
	assert b.last_n(2) == 'de'
	assert b.last_n(5) == 'abcde'
	assert b.after(-1) == 'abcde'
	assert b.after(-99) == 'abcde'
	assert b.after(0) == 'abcde'
	assert b.after(2) == 'cde'
	assert b.after(5) == ''
	check_rest(mut b, 'abcde')
}

// the example from https://github.com/vlang/v/issues/30044
fn test_issue_30044() {
	mut b := strings.new_builder(5)
	for i in 0 .. 5 {
		b.write_u8(u8(`a` + i))
	}
	assert b.cut_last(99) == 'abcde'
	assert b.len == 0
	mut d := strings.new_builder(5)
	for i in 0 .. 5 {
		d.write_u8(u8(`a` + i))
	}
	d.go_back(99)
	assert d.len == 0
	d.write_u8(`z`)
	assert d.len == 1
	assert d.str() == 'z'
	mut c := strings.new_builder(4)
	for i in 0 .. 4 {
		c.write_u8(u8(`a` + i))
	}
	assert c.spart(3, 5) == 'd'
	assert c.len == 4
}
