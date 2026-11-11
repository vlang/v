// string.bytes() and pointer-free single-element literals skip zeroing storage they copy
// over completely. Contents, metadata and independence must match zeroed allocations.
fn reference_bytes(s string) []u8 {
	mut out := []u8{len: s.len}
	for i in 0 .. s.len {
		out[i] = s[i]
	}
	return out
}

fn test_bytes_contents_and_metadata() {
	for n in [0, 1, 2, 7, 15, 16, 17, 31, 32, 33, 255, 4096, 65537] {
		mut raw := []u8{len: n}
		for i in 0 .. n {
			raw[i] = u8((i * 131 + 7) % 256)
		}
		s := raw.bytestr()
		b := s.bytes()
		assert b == reference_bytes(s), 'n=${n}'
		assert b.len == n
		assert b.cap == n
		zeroed := []u8{len: n}
		assert b.flags == zeroed.flags, 'n=${n}'
		assert b.element_size == 1
	}
}

fn test_bytes_embedded_nul_and_utf8() {
	assert 'a\x00b'.bytes() == [u8(`a`), 0, `b`]
	assert '\x00'.bytes() == [u8(0)]
	assert 'é∑'.bytes() == [u8(0xc3), 0xa9, 0xe2, 0x88, 0x91]
	assert ''.bytes() == []u8{}
}

fn test_bytes_result_is_independent() {
	s := 'hello world'
	mut a := s.bytes()
	mut b := s.bytes()
	a[0] = `H`
	assert s == 'hello world'
	assert b[0] == `h`
	b << `!`
	b << `?`
	assert b.bytestr() == 'hello world!?'
	assert a.bytestr() == 'Hello world'
	c := a.clone()
	a[1] = `E`
	assert c.bytestr() == 'Hello world'
	a.clear()
	assert a.len == 0
	assert s.bytes().bytestr() == s
}

fn test_bytes_grow_after_full_copy() {
	mut b := 'abc'.bytes()
	for _ in 0 .. 1000 {
		b << `x`
	}
	assert b.len == 1003
	assert b[..3].bytestr() == 'abc'
	assert b[3..].all(it == `x`)
}

fn test_single_element_noscan_literal() {
	for x in [0, 1, -5, 1 << 40] {
		mut a := [x]
		assert a.len == 1
		assert a.cap == 1
		assert a[0] == x
		a << 7
		assert a == [x, 7]
	}
	f := 2.5
	assert [f] == [2.5]
	c := u8(9)
	mut bs := [c]
	bs << 1
	assert bs == [u8(9), 1]
}
