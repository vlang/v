module asn1

fn test_null() {
	b := encode_null()
	assert b == [u8(0x05), 0x00]
	decode_null(b)!
}

fn test_enumerated() {
	b := encode_enumerated(5)
	assert b == [u8(0x0a), 0x01, 0x05]
	assert decode_enumerated(b)! == 5
}

fn test_utf8string() {
	b := encode_utf8string('hi')
	assert decode_utf8string(b)! == 'hi'
}

fn test_bitstring() {
	b := encode_bitstring([u8(0xf0)], 4)!
	data, unused := decode_bitstring(b)!
	assert data == [u8(0xf0)]
	assert unused == 4
}

fn test_oid_rsa() {
	// Well-known RSA OID prefix 1.2.840.113549.
	b := encode_oid('1.2.840.113549')!
	assert b == [u8(0x06), 0x06, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d]
	assert decode_oid(b)! == '1.2.840.113549'
}

fn test_oid_common_name() {
	b := encode_oid('2.5.4.3')!
	assert b == [u8(0x06), 0x03, 0x55, 0x04, 0x03]
	assert decode_oid(b)! == '2.5.4.3'
}

fn test_set() {
	parts := [encode_int(1), encode_int(2)]
	b := encode_set(parts)
	out := decode_set(b)!
	assert out.len == 2
	assert decode_int(out[0])! == 1
	assert decode_int(out[1])! == 2
}

fn test_explicit() {
	inner := encode_int(5)
	b := encode_explicit(0, inner)!
	assert b[0] == 0xa0
	// Unwrap manually: content must equal inner.
	_, content := unwrap_for_test(b)
	assert content == inner
}

fn unwrap_for_test(data []u8) (u8, []u8) {
	return data[0], data[2..]
}

fn test_utctime() {
	b := encode_utctime('300101000000Z')!
	assert b[0] == 0x17
	assert decode_utctime(b)! == '300101000000Z'
	if _ := encode_utctime('nope') {
		assert false, 'expected time error'
	} else {
		assert err.msg().contains('utctime')
	}
}

fn test_generalizedtime() {
	b := encode_generalizedtime('20300101000000Z')!
	assert b[0] == 0x18
	assert decode_generalizedtime(b)! == '20300101000000Z'
}
