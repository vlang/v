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

fn test_next() {
	mut s := textscanner.new('abc')
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
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
}

fn test_skip_n() {
	mut s := textscanner.new('abc')
	s.skip_n(2)
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_peek() {
	mut s := textscanner.new('abc')
	assert s.peek() == `a`
	assert s.peek() == `a`
	assert s.peek() == `a`
	//
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_peek_n() {
	mut s := textscanner.new('abc')
	assert s.peek_n(0) == `a`
	assert s.peek_n(1) == `b`
	assert s.peek_n(2) == `c`
	assert s.peek_n(3) == -1
	assert s.peek_n(4) == -1
	//
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
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

fn test_peek_back_n() {
	mut s := textscanner.new('abc')
	s.goto_end()
	assert s.peek_back_n(0) == `c`
	assert s.peek_back_n(1) == `b`
	assert s.peek_back_n(2) == `a`
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

fn test_goto_end() {
	mut s := textscanner.new('abc')
	s.goto_end()
	assert s.current() == `c`
}
