module binary

@[wire: 'little_endian']
struct WireHeader {
	magic   u16
	version u8
	length  u32
}

@[wire: 'big_endian']
struct WireHeaderBE {
	magic   u16
	version u8
	length  u32
}

enum WireKind as u8 {
	alpha = 1
	beta  = 2
}

enum WireWide as u16 {
	x = 0x1234
}

@[wire: 'little_endian']
struct WireInnerLE {
	tag WireKind
	val u32
}

@[wire: 'big_endian']
struct WireInnerBE {
	tag WireKind
	val u32
}

@[wire: 'little_endian']
struct WirePacket {
	kind   WireKind
	wide   WireWide
	flag   bool
	small  i8
	big    i64
	ratio  f32
	prec   f64
	data   [4]u8
	values [2]u16
	inner  WireInnerLE
	be     WireInnerBE
}

fn test_wire_little_endian_vector() {
	h := WireHeader{
		magic:   0x0102
		version: 0x03
		length:  0x04050607
	}
	enc := encode_wire(h)
	assert enc == [u8(0x02), 0x01, 0x03, 0x07, 0x06, 0x05, 0x04]
	assert wire_size(h) == 7
	assert decode_wire[WireHeader](enc)! == h
}

fn test_wire_big_endian_vector() {
	h := WireHeaderBE{
		magic:   0x0102
		version: 0x03
		length:  0x04050607
	}
	enc := encode_wire(h)
	assert enc == [u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
	assert wire_size(h) == 7
	assert decode_wire[WireHeaderBE](enc)! == h
}

fn test_wire_has_no_padding() {
	// A C layout pads `version` to 4 bytes; the wire layout is exactly 7.
	assert wire_size(WireHeader{}) == 7
	assert encode_wire(WireHeader{}).len == 7
	assert sizeof(WireHeader) == 8
}

fn test_wire_scalars_enums_arrays() {
	p := WirePacket{
		kind:   .beta
		wide:   .x
		flag:   true
		small:  -5
		big:    -0x0102030405060708
		ratio:  f32(1.5)
		prec:   2.5
		data:   [u8(1), 2, 3, 4]!
		values: [u16(0x0102), 0x0304]!
		inner:  WireInnerLE{.alpha, 0x0A0B0C0D}
		be:     WireInnerBE{.beta, 0x0A0B0C0D}
	}
	enc := encode_wire(p)
	assert enc[0] == 2
	assert enc[1..3] == [u8(0x34), 0x12]
	assert enc[3] == 1
	assert enc[4] == 0xFB
	assert enc[5..13] == [u8(0xF8), 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE]
	assert enc[13..17] == [u8(0x00), 0x00, 0xC0, 0x3F]
	assert enc[17..25] == [u8(0x00), 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x40]
	assert enc[25..29] == [u8(1), 2, 3, 4]
	assert enc[29..33] == [u8(0x02), 0x01, 0x04, 0x03]
	assert enc.len == 43
	assert wire_size(p) == 43
	assert decode_wire[WirePacket](enc)! == p
}

fn test_wire_nested_byte_order() {
	// Each nested wire struct keeps its own declared byte order.
	le := encode_wire(WireInnerLE{.beta, 0x0A0B0C0D})
	assert le == [u8(2), 0x0D, 0x0C, 0x0B, 0x0A]
	be := encode_wire(WireInnerBE{.beta, 0x0A0B0C0D})
	assert be == [u8(2), 0x0A, 0x0B, 0x0C, 0x0D]
	assert decode_wire[WireInnerLE](le)! == WireInnerLE{.beta, 0x0A0B0C0D}
	assert decode_wire[WireInnerBE](be)! == WireInnerBE{.beta, 0x0A0B0C0D}
}

fn test_wire_truncated_fails() {
	enc := encode_wire(WireHeader{
		magic:   0x0102
		version: 0x03
		length:  0x04050607
	})
	for cut in [0, 1, 3, 6] {
		if _ := decode_wire[WireHeader](enc[..cut]) {
			assert false, 'decode must fail for ${cut} of 7 bytes'
		} else {
			assert err.msg().contains('out of range')
		}
	}
	if _ := decode_wire[WirePacket](enc) {
		assert false, 'decode must fail for a short buffer'
	} else {
		assert err.msg().contains('out of range')
	}
}

fn padded(enc []u8) []u8 {
	mut out := enc.clone()
	out << u8(0)
	return out
}

fn test_wire_trailing_bytes_fail() {
	enc := encode_wire(WireHeader{
		magic:   0x0102
		version: 0x03
		length:  0x04050607
	})
	if _ := decode_wire[WireHeader](padded(enc)) {
		assert false, 'decode must fail for trailing bytes'
	} else {
		assert err.msg().contains('trailing bytes')
	}
	// Empty input for an empty-size mismatch also fails.
	if _ := decode_wire[WireHeader]([]u8{}) {
		assert false, 'decode must fail for empty input'
	} else {
		assert err.msg().contains('out of range')
	}
}
