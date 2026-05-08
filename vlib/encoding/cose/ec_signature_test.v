// Tests for ec_signature.v — DER ↔ raw R||S conversion. The reference
// values come from RFC 6979 (ECDSA deterministic signatures) and from
// hand-built DER blobs that exercise the leading-zero / MSB-set edge
// cases of the format.
module cose

import encoding.hex

fn test_der_to_raw_p256_roundtrip() {
	// R/S of length exactly 32 bytes, no DER padding needed.
	r := hex.decode('6520BBAF2081D7E0ED0F95F76EB0733D667005F7467CEC4B87B9381A6BA1EDE8')!
	s := hex.decode('00DF29F32A37230F39A842A54821FDD223092819D7728EFB9D3A0080B75380B')!
	mut raw := []u8{}
	raw << r
	raw << s
	assert raw.len == 64

	der := raw_to_der(raw, 32)!
	back := der_to_raw(der, 32)!
	assert back == raw
}

fn test_der_to_raw_strips_sign_byte() {
	// DER integer with leading 0x00 inserted because R's MSB is 1.
	// SEQUENCE (0x30) of two INTEGERs:
	//   0x02 0x21 0x00 R...   (33-byte INTEGER, first content byte 0x00)
	//   0x02 0x20 S...        (32-byte INTEGER)
	r_padded := hex.decode('00FFEEDDCCBBAA99887766554433221100FFEEDDCCBBAA99887766554433221100')!
	s := hex.decode('1122334455667788990011223344556677889900112233445566778899001122')!
	mut der := []u8{}
	der << 0x30
	der << u8(2 + r_padded.len + 2 + s.len)
	der << 0x02
	der << u8(r_padded.len)
	der << r_padded
	der << 0x02
	der << u8(s.len)
	der << s
	raw := der_to_raw(der, 32)!
	assert raw.len == 64
	// R is the original 32 high bytes (sign-disambiguation 0x00 dropped),
	// S is the 32-byte original.
	assert raw[..32] == r_padded[1..]
	assert raw[32..] == s
}

fn test_raw_to_der_inserts_sign_byte_when_msb_set() {
	// Build a raw signature whose R has MSB set; the resulting DER must
	// contain a leading 0x00 to keep the integer positive.
	mut raw := []u8{len: 64}
	raw[0] = 0xff // R[0] MSB set
	raw[32] = 0x01 // S[0] no MSB
	der := raw_to_der(raw, 32)!
	// Find the first INTEGER content byte (after 0x30 LEN 0x02 INTLEN).
	assert der[0] == 0x30
	mut idx := 2 // skip SEQUENCE tag and length
	assert der[idx] == 0x02
	int_len := int(der[idx + 1])
	assert int_len == 33 // 32 bytes + leading 0x00
	assert der[idx + 2] == 0x00
	assert der[idx + 3] == 0xff
}

fn test_raw_to_der_strips_internal_leading_zero() {
	// Raw R that already has a leading 0x00 (the high byte of the
	// curve scalar happens to be zero) must NOT keep it in DER beyond
	// what the sign-disambiguation rule requires.
	mut raw := []u8{len: 64}
	raw[1] = 0x42 // R = 00 42 00 ... — DER should encode as 0x02 0x1F 0x42 ...
	raw[32] = 0x01 // S = 01 00 ...
	der := raw_to_der(raw, 32)!
	assert der[0] == 0x30
	idx := 2
	assert der[idx] == 0x02
	int_len := int(der[idx + 1])
	// R magnitude after stripping leading zeros is 31 bytes: starts at
	// raw[1]=0x42 and ends at raw[31]=0x00.
	assert int_len == 31
	assert der[idx + 2] == 0x42
}

fn test_der_to_raw_left_pads_short_integer() {
	// DER R with 30 effective bytes (leading zeros in scalar) must be
	// left-padded to 32 in the raw form.
	r_short := hex.decode('1122334455667788991122334455667788991122334455667788991122334455')!
	r_short_30 := r_short[2..] // 30 bytes
	s := hex.decode('1122334455667788990011223344556677889900112233445566778899001122')!
	mut der := []u8{}
	der << 0x30
	der << u8(2 + r_short_30.len + 2 + s.len)
	der << 0x02
	der << u8(r_short_30.len)
	der << r_short_30
	der << 0x02
	der << u8(s.len)
	der << s
	raw := der_to_raw(der, 32)!
	assert raw.len == 64
	assert raw[0] == 0x00
	assert raw[1] == 0x00
	assert raw[2..32] == r_short_30
}

fn test_raw_to_der_rejects_wrong_length() {
	bad := []u8{len: 60} // P-256 expects 64
	if _ := raw_to_der(bad, 32) {
		assert false, 'should have rejected wrong-length raw'
	} else {
		assert err is MalformedMessage
	}
}

fn test_der_to_raw_rejects_truncated() {
	bad := [u8(0x30), 0x05, 0x02, 0x01, 0x01]
	if _ := der_to_raw(bad, 32) {
		assert false, 'should have rejected truncated DER'
	} else {
		assert err is MalformedMessage
	}
}

fn test_long_form_length_p521() {
	// P-521 signatures often exceed 0x7f bytes, so the SEQUENCE length
	// uses the long form (0x81 LEN). Build a minimal example and ensure
	// it round-trips.
	mut raw := []u8{len: 132}
	for i in 0 .. 132 {
		raw[i] = u8(i)
	}
	der := raw_to_der(raw, 66)!
	// The DER should start with 0x30 0x81 ... (long form, single byte).
	assert der[0] == 0x30
	assert der[1] == 0x81
	back := der_to_raw(der, 66)!
	assert back == raw
}
