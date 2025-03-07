import strings.textscanner

fn test_remaining() {
	mut s := textscanner.new('abc')
	assert s.remaining() == 3
	s.next()
	s.next()
	assert s.remaining() == 1
	s.next()
	assert s.remaining() == 0
	s.next()
	s.next()
	assert s.remaining() == 0
	s.reset()
	assert s.remaining() == 3
}

fn test_remaining_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.remaining() == 3
	s.next()
	s.next()
	assert s.remaining() == 1
	s.next()
	assert s.remaining() == 0
	s.next()
	s.next()
	assert s.remaining() == 0
	s.reset()
	assert s.remaining() == 3
}

fn xtest_next() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
	assert s.next() == -1
	assert s.next() == -1
}

fn test_next_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	assert s.next() == `语`
	assert s.next() == `言`
	assert s.next() == -1
	assert s.next() == -1
	assert s.next() == -1
}

fn test_skip() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	s.skip()
	assert s.next() == `c`
	assert s.next() == -1

	s.reset()
	assert s.peek() == `a`
	s.skip()
	assert s.peek() == `b`
	s.skip()
	assert s.peek() == `c`
	s.skip()
	assert s.peek() == -1
}

fn test_skip_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	s.skip()
	assert s.next() == `言`
	assert s.next() == -1

	s.reset()
	assert s.peek() == `v`
	s.skip()
	assert s.peek() == `语`
	s.skip()
	assert s.peek() == `言`
	s.skip()
	assert s.peek() == -1
}

fn test_skip_n() {
	mut s := textscanner.new('abc')
	s.skip_n(2)
	assert s.next() == `c`
	assert s.next() == -1

	s.reset()
	assert s.peek() == `a`
	s.skip_n(2)
	assert s.peek() == `c`
	s.skip_n(2)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `a`
	s.skip_n(3)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `a`
	s.skip_n(4)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `a`
	s.skip_n(-3)
	assert s.peek() == `a`
}

fn test_skip_n_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	s.skip_n(2)
	assert s.next() == `言`
	assert s.next() == -1

	s.reset()
	assert s.peek() == `v`
	s.skip_n(2)
	assert s.peek() == `言`
	s.skip_n(2)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `v`
	s.skip_n(3)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `v`
	s.skip_n(4)
	assert s.peek() == -1

	s.reset()
	assert s.peek() == `v`
	s.skip_n(-3)
	assert s.peek() == `v`
}

fn test_peek() {
	mut s := textscanner.new('abc')
	assert s.peek() == `a`
	assert s.peek() == `a`
	assert s.peek() == `a`

	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_peek_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.peek() == `v`
	assert s.peek() == `v`
	assert s.peek() == `v`

	assert s.next() == `v`
	assert s.next() == `语`
	assert s.next() == `言`
	assert s.next() == -1
}

fn test_peek_n() {
	mut s := textscanner.new('abc')
	assert s.peek_n(0) == `a`
	assert s.peek_n(1) == `b`
	assert s.peek_n(2) == `c`
	assert s.peek_n(3) == -1
	assert s.peek_n(4) == -1

	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_peek_n_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.peek_n(0) == `v`
	assert s.peek_n(1) == `语`
	assert s.peek_n(2) == `言`
	assert s.peek_n(3) == -1
	assert s.peek_n(4) == -1

	assert s.next() == `v`
	assert s.next() == `语`
	assert s.next() == `言`
	assert s.next() == -1
}

fn test_back() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	s.back()
	assert s.next() == `a`
	assert s.next() == `b`
	s.back()
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_back_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	s.back()
	assert s.next() == `v`
	assert s.next() == `语`
	s.back()
	assert s.next() == `语`
	assert s.next() == `言`
	assert s.next() == -1
}

fn test_back_n() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	s.back_n(10)
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	s.back_n(2)
	assert s.next() == `b`
}

fn test_back_n_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	s.back_n(10)
	assert s.next() == `v`
	assert s.next() == `语`
	assert s.next() == `言`
	s.back_n(2)
	assert s.next() == `语`
}

fn test_peek_back() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	assert s.next() == `b`
	// check that calling .peek_back() multiple times
	// does not change the state:
	assert s.peek_back() == `a`
	assert s.peek_back() == `a`
	assert s.peek_back() == `a`
	// advance, then peek_back again:
	assert s.next() == `c`
	assert s.peek_back() == `b`
	// peeking before the start:
	s.reset()
	assert s.peek_back() == -1
	// peeking right at the end:
	s.goto_end()
	assert s.peek_back() == `b`
}

fn test_peek_back_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	assert s.next() == `语`
	// check that calling .peek_back() multiple times
	// does not change the state:
	assert s.peek_back() == `v`
	assert s.peek_back() == `v`
	assert s.peek_back() == `v`
	// advance, then peek_back again:
	assert s.next() == `言`
	assert s.peek_back() == `语`
	// peeking before the start:
	s.reset()
	assert s.peek_back() == -1
	// peeking right at the end:
	s.goto_end()
	assert s.peek_back() == `语`
}

fn test_peek_back_n() {
	mut s := textscanner.new('abc')
	s.goto_end()
	assert s.peek_back_n(0) == `c`
	assert s.peek_back_n(1) == `b`
	assert s.peek_back_n(2) == `a`
	assert s.peek_back_n(3) == -1
	assert s.peek_back_n(4) == -1
}

fn test_peek_back_n_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	s.goto_end()
	assert s.peek_back_n(0) == `言`
	assert s.peek_back_n(1) == `语`
	assert s.peek_back_n(2) == `v`
	assert s.peek_back_n(3) == -1
	assert s.peek_back_n(4) == -1
}

fn test_reset() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	s.next()
	s.next()
	assert s.next() == -1
	s.reset()
	assert s.next() == `a`
}

fn test_reset_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.next() == `v`
	s.next()
	s.next()
	assert s.next() == -1
	s.reset()
	assert s.next() == `v`
}

fn test_current() {
	mut s := textscanner.new('abc')
	assert s.current() == -1
	assert s.next() == `a`
	assert s.current() == `a`
	assert s.current() == `a`
	assert s.peek_back() == -1
	assert s.next() == `b`
	assert s.current() == `b`
	assert s.current() == `b`
	assert s.peek_back() == `a`
	assert s.next() == `c`
	assert s.current() == `c`
	assert s.next() == -1
	assert s.current() == `c`
	assert s.next() == -1
	assert s.current() == `c`
	s.reset()
	assert s.current() == -1
	assert s.next() == `a`
	assert s.current() == `a`
}

fn test_current_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.current() == -1
	assert s.next() == `v`
	assert s.current() == `v`
	assert s.current() == `v`
	assert s.peek_back() == -1
	assert s.next() == `语`
	assert s.current() == `语`
	assert s.current() == `语`
	assert s.peek_back() == `v`
	assert s.next() == `言`
	assert s.current() == `言`
	assert s.next() == -1
	assert s.current() == `言`
	assert s.next() == -1
	assert s.current() == `言`
	s.reset()
	assert s.current() == -1
	assert s.next() == `v`
	assert s.current() == `v`
}

fn test_goto_end() {
	mut s := textscanner.new('abc')
	s.goto_end()
	assert s.current() == `c`
}

fn test_goto_end_rune() {
	mut s := textscanner.new('v语言', force_rune_mode: true)
	s.goto_end()
	assert s.current() == `言`
}

fn test_skip_whitespace() {
	mut s := textscanner.new('abc   d  \n   xyz')
	assert s.current() == -1
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	s.skip_whitespace()
	assert s.next() == `d`
	s.skip_whitespace()
	assert s.next() == `x`
	assert s.next() == `y`
	assert s.next() == `z`
}

fn test_skip_whitespace_rune() {
	mut s := textscanner.new('v语言   ♥  \n   大家好', force_rune_mode: true)
	assert s.current() == -1
	assert s.next() == `v`
	assert s.next() == `语`
	assert s.next() == `言`
	s.skip_whitespace()
	assert s.next() == `♥`
	s.skip_whitespace()
	assert s.next() == `大`
	assert s.next() == `家`
	assert s.next() == `好`
}

fn test_peek_u8() {
	mut s := textscanner.new('abc')
	assert s.peek_u8() == `a`
	assert !s.peek_u8().is_digit()
	assert s.next() == `a`
	assert s.peek_u8() == `b`
}

fn test_peek_u8_rune() {
	// maybe for rune mode, we should not use `peek_u8()`
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.peek_u8() == u8(`v`)
	assert !s.peek_u8().is_digit()
	assert s.next() == u8(`v`)
	assert s.peek_u8() == u8(`语`)
}

fn test_peek_n_u8() {
	mut s := textscanner.new('abc')
	assert s.peek_n_u8(0) == `a`
	assert s.peek_n_u8(1) == `b`
	assert s.peek_n_u8(2) == `c`
	assert s.peek_n_u8(3) == 0
	assert s.peek_n_u8(4) == 0
}

fn test_peek_n_u8_rune() {
	// maybe for rune mode, we should not use `peek_n_u8()`
	mut s := textscanner.new('v语言', force_rune_mode: true)
	assert s.peek_n_u8(0) == u8(`v`)
	assert s.peek_n_u8(1) == u8(`语`)
	assert s.peek_n_u8(2) == u8(`言`)
	assert s.peek_n_u8(3) == 0
	assert s.peek_n_u8(4) == 0
}

fn test_next_line() {
	mut s := textscanner.new('abc\r\n123\n\n8')
	line1, end1 := s.next_line()
	assert line1 == 'abc'
	assert end1 == true

	line2, end2 := s.next_line()
	assert line2 == '123'
	assert end2 == true

	line3, end3 := s.next_line()
	assert line3 == ''
	assert end3 == true

	line4, end4 := s.next_line()
	assert line4 == '8'
	assert end4 == false

	line5, end5 := s.next_line()
	assert line5 == ''
	assert end5 == false
}

fn test_next_line_rune() {
	mut s := textscanner.new('v语言\r\n大家好\n\n♥', force_rune_mode: true)
	line1, end1 := s.next_line()
	assert line1 == 'v语言'
	assert end1 == true

	line2, end2 := s.next_line()
	assert line2 == '大家好'
	assert end2 == true

	line3, end3 := s.next_line()
	assert line3 == ''
	assert end3 == true

	line4, end4 := s.next_line()
	assert line4 == '♥'
	assert end4 == false

	line5, end5 := s.next_line()
	assert line5 == ''
	assert end5 == false
}

fn test_read_until() {
	mut s := textscanner.new('abc\r\n12|3#')
	t1 := s.read_until('|') or { panic(err) }
	assert t1 == 'abc\r\n12'

	t2 := s.read_until('#') or { panic(err) }
	assert t2 == '3'
	t3 := s.read_until('#') or {
		assert err.msg() == 'already at EOF'
		'not exist'
	}
	assert t3 == 'not exist'

	mut ss := textscanner.new('abc\r\n12|3#')
	tt1 := ss.read_until('|#') or { panic(err) }
	assert tt1 == 'abc\r\n12'

	tt2 := ss.read_until('|#') or { panic(err) }
	assert tt2 == '3'
	tt3 := ss.read_until('|#') or {
		assert err.msg() == 'already at EOF'
		'not exist'
	}
	assert tt3 == 'not exist'
}

fn test_read_until_rune() {
	mut s := textscanner.new('v语言♥\r\n大家好|♥3#', force_rune_mode: true)
	t1 := s.read_until('♥') or { panic(err) }
	assert t1 == 'v语言'

	t2 := s.read_until('#') or { panic(err) }
	assert t2 == '\r\n大家好|♥3'
	t3 := s.read_until('#') or {
		assert err.msg() == 'already at EOF'
		'已经到结尾了'
	}
	assert t3 == '已经到结尾了'

	mut ss := textscanner.new('v语言♥\r\n大家好|♥3#', force_rune_mode: true)
	tt1 := ss.read_until('♥#') or { panic(err) }
	assert tt1 == 'v语言'

	tt2 := ss.read_until('♥#') or { panic(err) }
	assert tt2 == '\r\n大家好|'
	tt3 := ss.read_until('♥#') or { panic(err) }
	assert tt3 == '3'
	tt4 := ss.read_until('♥#') or {
		assert err.msg() == 'already at EOF'
		'已经到结尾了'
	}
	assert tt4 == '已经到结尾了'
}
