// `s.trim_space() == 'lit'` and `s.to_lower_ascii() == 'lit'` compare in place; every other
// shape keeps the allocating path. Both must agree with the materialized normalized string.
type Text = string

fn (t Text) trim_space() string {
	return 'custom'
}

@[heap]
struct Counter {
mut:
	n int
}

fn trimmed(s string) string {
	return s.trim_space()
}

fn lowered(s string) string {
	return s.to_lower_ascii()
}

fn trim_cases() []string {
	return ['', ' ', '\t\n\v\f\r ', 'x', ' x', 'x ', ' x ', '\tx\n', 'x y', ' x  y ', 'xx', 'y',
		'\x00x', 'x\x00', ' \x00 ', '\xc2\xa0x', 'x\xc2\xa0', 'é', ' é ', 'ok', ' ok\r\n']
}

fn test_trim_space_eq_matches_reference() {
	for s in trim_cases() {
		ref := trimmed(s)
		// Spelled out so each comparison is lowered against a literal.
		assert (s.trim_space() == '') == (ref == ''), '${s.bytes()}'
		assert (s.trim_space() == 'x') == (ref == 'x'), '${s.bytes()}'
		assert (s.trim_space() == 'x y') == (ref == 'x y'), '${s.bytes()}'
		assert (s.trim_space() == 'xx') == (ref == 'xx'), '${s.bytes()}'
		assert (s.trim_space() == 'ok') == (ref == 'ok'), '${s.bytes()}'
		assert (s.trim_space() == '\x00x') == (ref == '\x00x'), '${s.bytes()}'
		assert (s.trim_space() == 'é') == (ref == 'é'), '${s.bytes()}'
		assert (s.trim_space() == '\xc2\xa0x') == (ref == '\xc2\xa0x'), '${s.bytes()}'
		assert (s.trim_space() != 'x') == (ref != 'x'), '${s.bytes()}'
		assert ('x' == s.trim_space()) == (ref == 'x'), '${s.bytes()}'
		assert ('' != s.trim_space()) == (ref != ''), '${s.bytes()}'
	}
}

fn test_to_lower_ascii_eq_every_byte() {
	for i in 0 .. 256 {
		s := [u8(i)].bytestr()
		ref := lowered(s)
		assert (s.to_lower_ascii() == 'a') == (ref == 'a'), 'byte ${i}'
		assert (s.to_lower_ascii() == 'A') == (ref == 'A'), 'byte ${i}'
		assert (s.to_lower_ascii() == '\x00') == (ref == '\x00'), 'byte ${i}'
		assert (s.to_lower_ascii() != 'z') == (ref != 'z'), 'byte ${i}'
		assert ('[' == s.to_lower_ascii()) == (ref == '['), 'byte ${i}'
	}
}

fn test_to_lower_ascii_eq_cases() {
	for s in ['host', 'HOST', 'HoSt', 'hos', 'hostt', '', 'HÖST', 'hÖst', 'h\x00st', 'Ꮋost'] {
		ref := lowered(s)
		assert (s.to_lower_ascii() == 'host') == (ref == 'host'), s
		assert (s.to_lower_ascii() == 'HOST') == (ref == 'HOST'), s
		assert (s.to_lower_ascii() == '') == (ref == ''), s
		assert (s.to_lower_ascii() == 'hÖst') == (ref == 'hÖst'), s
		assert (s.to_lower_ascii() == 'h\x00st') == (ref == 'h\x00st'), s
		assert (s.to_lower_ascii() != 'host') == (ref != 'host'), s
		assert ('host' == s.to_lower_ascii()) == (ref == 'host'), s
	}
	// The literal side is not lowered.
	assert 'HOST'.to_lower_ascii() != 'HOST'
	// Unicode `to_lower` keeps its own semantics.
	assert 'ÄB'.to_lower() == 'äb'
	assert 'ÄB'.to_lower_ascii() != 'äb'
}

fn test_receiver_evaluated_once() {
	mut c := &Counter{}
	next := fn [mut c] () string {
		c.n++
		return ' Ok '
	}
	assert next().trim_space() == 'Ok'
	assert c.n == 1
	assert next().to_lower_ascii() != ' ok'
	assert c.n == 2
	assert ' ok ' == next().to_lower_ascii()
	assert c.n == 3
}

fn test_fallbacks() {
	t := Text(' x ')
	assert t.trim_space() == 'custom'
	s := ' Host '
	// Non-literal operand and a retained normalized value keep the ordinary path.
	want := 'host'
	assert s.trim_space().to_lower_ascii() == want
	norm := s.trim_space()
	assert norm == 'Host'
	assert norm.len == 4
	f := string.trim_space
	assert f(s) == 'Host'
}
