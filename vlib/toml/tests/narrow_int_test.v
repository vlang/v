import toml

struct Narrow {
	a u8
	b u16
	c u32
	d i8
	e i16
	f i32
}

struct Defaults {
	port    u16 = 8080
	retries u8  = 3
	offset  i8  = 7
}

struct NarrowCollections {
	ports   []u16
	by_name map[string]i32
}

const toml_text = 'a = 250
b = 65535
c = 4294967295
d = -128
e = -32768
f = -2147483648
'

fn test_decode_narrow_ints() {
	n := toml.decode[Narrow](toml_text) or { panic(err) }
	assert n.a == u8(250)
	assert n.b == u16(65535)
	assert n.c == u32(4294967295)
	assert n.d == i8(-128)
	assert n.e == i16(-32768)
	assert n.f == i32(-2147483648)
}

fn test_encode_narrow_ints() {
	// Narrow integers must be encoded as TOML integers, not as quoted strings.
	n := Narrow{250, 65535, 4294967295, -128, -32768, -2147483648}
	doc := toml.parse_text(toml.encode[Narrow](n)) or { panic(err) }
	assert doc.value('a').i64() == 250
	assert doc.value('b').i64() == 65535
	assert doc.value('c').i64() == 4294967295
	assert doc.value('d').i64() == -128
	assert !toml.encode[Narrow](n).contains('"')
}

fn test_encode_decode_narrow_ints() {
	n := Narrow{7, 8080, 70000, -1, -300, 123456}
	assert toml.decode[Narrow](toml.encode[Narrow](n))! == n
}

fn test_decode_narrow_int_out_of_range() {
	// Values that the field type cannot represent are skipped, so the declared
	// default value is kept.
	n := toml.decode[Defaults]('port = 99999\nretries = 999\noffset = -999') or {
		panic(err)
	}
	assert n.port == u16(8080)
	assert n.retries == u8(3)
	assert n.offset == i8(7)
}

fn test_decode_narrow_int_negative_into_unsigned() {
	n := toml.decode[Defaults]('port = -1') or { panic(err) }
	assert n.port == u16(8080)
}

fn test_decode_narrow_ints_in_collections() {
	c := toml.decode[NarrowCollections]('ports = [80, 443, 65535]\n\n[by_name]\na = -1\nb = 100000\n') or {
		panic(err)
	}
	assert c.ports == [u16(80), u16(443), u16(65535)]
	assert c.by_name == {
		'a': i32(-1)
		'b': i32(100000)
	}
}

fn test_decode_narrow_ints_in_collections_out_of_range() {
	c := toml.decode[NarrowCollections]('ports = [80, 99999]\n\n[by_name]\na = 1\nb = 9999999999\n') or {
		panic(err)
	}
	// The out of range elements are skipped.
	assert c.ports == [u16(80)]
	assert c.by_name == {
		'a': i32(1)
	}
}

fn test_decode_narrow_int_rejects_non_integers() {
	// `Any.i64()` returns 0 for these; that 0 must not replace the declared default.
	for text in ['port = inf', 'port = -inf', 'port = nan', 'port = 1.5', 'port = true', 'port = "abc"',
		'port = ""', 'port = [1]', 'port = { a = 1 }', 'port = 1979-05-27'] {
		n := toml.decode[Defaults](text) or { panic(err) }
		assert n.port == u16(8080), text
	}
}

fn test_decode_narrow_int_from_numeric_string() {
	// Older versions of `toml.encode` wrote narrow integers as quoted strings.
	n := toml.decode[Defaults]('port = "9090"\nretries = "5"\noffset = "-2"') or { panic(err) }
	assert n.port == u16(9090)
	assert n.retries == u8(5)
	assert n.offset == i8(-2)
	m := toml.decode[Defaults]('port = "99999"\nretries = "-1"\noffset = "1.5"') or {
		panic(err)
	}
	assert m.port == u16(8080)
	assert m.retries == u8(3)
	assert m.offset == i8(7)
}

fn test_decode_narrow_ints_in_collections_rejects_non_integers() {
	c := toml.decode[NarrowCollections]('ports = [80, inf, -inf, nan, 1.5, true, "443", "x"]\n\n[by_name]\na = 1\nb = inf\nc = nan\nd = "-7"\ne = 2.5\n') or {
		panic(err)
	}
	assert c.ports == [u16(80), u16(443)]
	assert c.by_name == {
		'a': i32(1)
		'd': i32(-7)
	}
}

struct OtherInts {
	sizes  []usize          = [usize(7)]
	isizes map[string]isize = {
		'a': isize(5)
	}
	runes  []rune = [`x`]
}

fn test_decode_other_int_collections_keep_current() {
	// Only u8, u16, u32, i8, i16 and i32 are decoded as narrow integers. Collections
	// of the other integer types keep their current value, as before.
	o := toml.decode[OtherInts]('sizes = [1]\nrunes = [1]\n\n[isizes]\nb = 2\n') or {
		panic(err)
	}
	assert o.sizes == [usize(7)]
	assert o.isizes == {
		'a': isize(5)
	}
	assert o.runes == [`x`]
}

struct WideInts {
	u usize
	r rune
}

fn test_encode_other_ints() {
	w := WideInts{~usize(0), `🚀`}
	encoded := toml.encode[WideInts](w)
	// `usize` is not widened to `i64`, which would wrap its largest values.
	assert encoded.contains('${w.u}')
	// `rune` is still encoded as a string.
	assert encoded.contains('r = "🚀"')
}

struct Listen {
	port u16
}

struct Server {
	Listen
	name string
}

fn test_encode_and_decode_embedded_narrow_ints() {
	s := Server{Listen{8080}, 'web'}
	encoded := toml.encode[Server](s)
	assert encoded == 'port = 8080\nname = "web"'
	assert toml.decode[Server](encoded)! == s
}
