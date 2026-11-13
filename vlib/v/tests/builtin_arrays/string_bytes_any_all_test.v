// `s.bytes().any/all(pred)` with a pure byte predicate scans the string in place;
// everything else keeps the ordinary array path. Both must agree with a plain loop.
type ByteText = string

fn (t ByteText) bytes() []u8 {
	return [u8(`x`)]
}

@[heap]
struct Counter {
mut:
	n    int
	seen []u8
}

@[heap]
struct Holder {
mut:
	text string
}

fn reference_all(s string, f fn (u8) bool) bool {
	for c in s.bytes() {
		if !f(c) {
			return false
		}
	}
	return true
}

fn is_digit_byte(c u8) bool {
	return c >= `0` && c <= `9`
}

fn all_digits(s string) bool {
	return s.bytes().all(it >= `0` && it <= `9`)
}

fn has_control(s string) bool {
	return s.bytes().any(it < 32 || it == 127)
}

fn all_digits_shadowing_receiver(s string) bool {
	return s.bytes().all(|s| s.is_digit())
}

fn any_digit_shadowing_receiver(s string) bool {
	return s.bytes().any(|s| s.is_digit())
}

fn all_digits_ref(s &string) bool {
	return s.bytes().all(it.is_digit())
}

fn tail_hex(s string) bool {
	return s[1..].bytes().all(it.is_hex_digit() || it in [`-`, `_`])
}

fn generic_all[T](s T) bool {
	return s.bytes().all(it.is_alnum())
}

fn test_empty_and_single() {
	assert all_digits('')
	assert !has_control('')
	assert !''.bytes().any(true)
	assert ''.bytes().all(false)
	assert all_digits('7')
	assert !all_digits('x')
	assert has_control('\n')
	assert !has_control('a')
}

fn test_positions() {
	long := '1'.repeat(65536)
	assert all_digits(long)
	assert !all_digits('x' + long)
	assert !all_digits(long + 'x')
	assert !all_digits(long[..100] + 'x' + long[..100])
	assert has_control(long + '\x7f')
	assert !has_control(long)
}

fn test_lambda_parameter_shadows_receiver() {
	assert all_digits_shadowing_receiver('')
	assert all_digits_shadowing_receiver('012345')
	assert !all_digits_shadowing_receiver('x12')
	assert !all_digits_shadowing_receiver('12x')
	assert !any_digit_shadowing_receiver('')
	assert !any_digit_shadowing_receiver('xyz')
	assert any_digit_shadowing_receiver('0xy')
	assert any_digit_shadowing_receiver('xy0')
}

fn test_every_byte_value() {
	mut buf := []u8{len: 256}
	for i in 0 .. 256 {
		buf[i] = u8(i)
	}
	s := buf.bytestr()
	assert s.len == 256
	assert s.bytes().any(it == 0)
	assert s.bytes().any(it == 255)
	assert !s.bytes().all(it != 128)
	for i in 0 .. 256 {
		one := [u8(i)].bytestr()
		assert all_digits(one) == reference_all(one, is_digit_byte), 'byte ${i}'
		assert has_control(one) == (i < 32 || i == 127), 'byte ${i}'
		assert one.bytes().all(it.is_space()) == u8(i).is_space(), 'byte ${i}'
		assert one.bytes().all(~it == ~u8(i)), 'byte ${i}'
	}
}

fn test_embedded_nul_and_utf8() {
	assert '12\x0034'.bytes().any(it == 0)
	assert !all_digits('12\x0034')
	assert 'héllo'.bytes().any(it >= 0x80)
	assert !'héllo'.bytes().all(it.is_letter())
	assert 'héllo'.bytes().all(it != `x`)
}

fn test_receivers() {
	mut s := '0123'
	assert all_digits_ref(&s)
	assert s.bytes().all(it.is_digit())
	s += 'z'
	assert !all_digits_ref(&s)
	assert !s.bytes().all(it.is_digit())
	assert tail_hex('xdead-beef_')
	assert !tail_hex('xdeag')
	assert tail_hex('x')
	h := Holder{
		text: 'abc'
	}
	assert h.text.bytes().all(it.is_letter())
	assert generic_all('abc123')
	assert !generic_all('abc 123')
	assert 'ABC'.bytes().all(|c| c.is_capital())
	assert !'ABc'.bytes().all(|c| c.is_capital())
}

fn test_receiver_evaluated_once() {
	mut c := &Counter{}
	next := fn [mut c] () string {
		c.n++
		return '42'
	}
	assert next().bytes().all(it.is_digit())
	assert c.n == 1
	assert !next().bytes().any(it == `x`)
	assert c.n == 2
}

fn test_fallback_effectful_predicate_visits_every_byte() {
	mut c := &Counter{}
	result := '12x45'.bytes().all(fn [mut c] (b u8) bool {
		c.n++
		return b.is_digit()
	})
	assert !result
	assert c.n == 5
	assert !'12x45'.bytes().all(is_digit_byte)
}

fn test_fallback_custom_bytes() {
	t := ByteText('123')
	assert !t.bytes().all(it.is_digit())
	assert t.bytes().all(it == `x`)
}

fn test_fallback_snapshot() {
	mut h := &Holder{
		text: 'aaa'
	}
	mut c := &Counter{}
	result := h.text.bytes().all(fn [mut h, mut c] (b u8) bool {
		c.seen << b
		h.text = 'b'
		return true
	})
	assert result
	assert c.seen == [u8(`a`), `a`, `a`]
	assert h.text == 'b'
}
