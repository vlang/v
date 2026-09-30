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
