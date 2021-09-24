import toml.input
import toml.scanner

const scan_input = input.Config{
	text: 'abc'
}

fn test_remaining() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
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
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
	assert s.next() == -1
	assert s.next() == -1
}

fn test_skip() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.next() == `a`
	s.skip()
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_skip_n() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	s.skip_n(2)
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_at() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.at() == `a`
	assert s.at() == `a`
	assert s.at() == `a`
	//
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_peek() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.peek(0) == `a`
	assert s.peek(1) == `b`
	assert s.peek(2) == `c`
	assert s.peek(3) == -1
	assert s.peek(4) == -1
	//
	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == -1
}

fn test_reset() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.next() == `a`
	s.next()
	s.next()
	assert s.next() == -1
	s.reset()
	assert s.next() == `a`
}
