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
	assert s.next() == scanner.end_of_text
	assert s.next() == scanner.end_of_text
	assert s.next() == scanner.end_of_text
}

fn test_skip() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.next() == `a`
	s.skip()
	assert s.next() == `c`
	assert s.next() == scanner.end_of_text
}

fn test_skip_n() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	s.skip_n(2)
	assert s.next() == `c`
	assert s.next() == scanner.end_of_text
}

fn test_at() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.at() == `a`
	assert s.at() == `a`
	assert s.at() == `a`

	assert s.next() == `a`
	assert s.next() == `b`
	assert s.next() == `c`
	assert s.next() == scanner.end_of_text
}

fn test_peek() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.peek(0) == `a`
	assert s.peek(1) == `b`
	assert s.peek(2) == `c`
	assert s.peek(3) == scanner.end_of_text
	assert s.peek(4) == scanner.end_of_text

	// peek(0) is the byte at the current position, not the previous one
	assert s.next() == `a`
	assert s.peek(0) == `b`
	assert s.peek(1) == `c`
	assert s.next() == `b`
	assert s.peek(0) == `c`
	assert s.peek(1) == scanner.end_of_text

	assert s.next() == `c`
	assert s.peek(0) == scanner.end_of_text
	assert s.next() == scanner.end_of_text
}

fn test_reset() {
	mut s := scanner.new_scanner(input: scan_input) or { panic(err) }
	assert s.next() == `a`
	s.next()
	s.next()
	assert s.next() == scanner.end_of_text
	s.reset()
	assert s.next() == `a`
}

const multiline_string_input = input.Config{
	text: '"""abc\r\ndef\n123"""'
}

fn test_multiline_string() {
	mut s := scanner.new_scanner(input: multiline_string_input) or { panic(err) }
	tok := s.scan()!
	assert tok.kind == .quoted
	assert tok.lit.contains('abc')
	assert tok.lit.contains('def')
	assert tok.lit.contains('123')
}

fn test_peek_negative_offsets() ! {
	mut s := scanner.new_scanner(input: scan_input)!
	assert s.peek(-1) == scanner.end_of_text
	assert s.peek(min_int) == scanner.end_of_text
	assert s.peek(max_int) == scanner.end_of_text
	s.next()
	assert s.peek(-1) == `a`
	assert s.peek(-2) == scanner.end_of_text
	s.next()
	assert s.peek(-1) == `b`
	assert s.peek(-2) == `a`
	s.next()
	assert s.peek(-1) == `c`
	assert s.peek(-3) == `a`
	assert s.peek(-4) == scanner.end_of_text
	assert s.peek(max_int) == scanner.end_of_text
}
