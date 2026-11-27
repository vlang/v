module tls

import rand

fn test_plain_record_roundtrip() {
	frag := [u8(1), 2, 3, 4, 5]
	r := PlainRecord{
		content_type: .handshake
		version:      tls_version_1_3
		fragment:     frag
	}
	enc := encode_plain_record(r)
	assert enc.len == 5 + frag.len
	got, n := decode_plain_record(enc) or { panic(err) }
	assert n == enc.len
	assert got.content_type == .handshake
	assert got.version == tls_version_1_3
	assert got.fragment == frag
}

fn test_plain_record_truncated() {
	_, _ = decode_plain_record([u8(22), 3, 3]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false
}

fn test_plain_record_too_large() {
	mut data := []u8{len: 5}
	data[0] = 23
	data[1] = 3
	data[2] = 3
	data[3] = 0x80 // length 0x8000 > limit
	data[4] = 0
	_, _ = decode_plain_record(data) or {
		assert err.msg().contains('too large')
		return
	}
	assert false
}

fn test_inner_roundtrip() {
	pt := InnerPlaintext{
		content:      [u8(10), 20, 30]
		content_type: .application_data
		padding_len:  3
	}
	enc := encode_inner(pt)
	dec := decode_inner(enc) or { panic(err) }
	assert dec.content == pt.content
	assert dec.content_type == .application_data
	assert dec.padding_len == 3
}

fn test_inner_no_padding() {
	pt := InnerPlaintext{
		content:      [u8(7), 8]
		content_type: .handshake
		padding_len:  0
	}
	enc := encode_inner(pt)
	dec := decode_inner(enc) or { panic(err) }
	assert dec.content == pt.content
	assert dec.content_type == .handshake
	assert dec.padding_len == 0
}

fn test_inner_empty_rejected() {
	decode_inner([]u8{}) or {
		assert err.msg().contains('empty')
		return
	}
	assert false
}

fn test_cipher_record_roundtrip() {
	mut frag := []u8{len: 40}
	for i in 0 .. frag.len {
		frag[i] = u8(i)
	}
	r := CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       frag
	}
	enc := encode_cipher_record(r)
	got, n := decode_cipher_record(enc) or { panic(err) }
	assert n == enc.len
	assert got.fragment == frag
}

fn test_alert_roundtrip() {
	ab := alert_bytes(.fatal, .handshake_failure)
	assert ab == [u8(2), 40]
	lvl, desc := parse_alert(ab) or { panic(err) }
	assert lvl == .fatal
	assert desc == .handshake_failure
}

fn test_alert_truncated() {
	parse_alert([u8(2)]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false
}

fn test_record_size_constants() {
	assert max_plaintext_len == 16384
	assert max_ciphertext_len == 16384 + 256
}

fn test_random_roundtrip() {
	for _ in 0 .. 64 {
		n := 1 + int(rand.intn(200) or { 0 })
		mut frag := []u8{len: n}
		for i in 0 .. n {
			frag[i] = u8(i * 37 + 7)
		}
		r := PlainRecord{
			content_type: .application_data
			version:      tls_version_1_3
			fragment:     frag
		}
		enc := encode_plain_record(r)
		got, consumed := decode_plain_record(enc) or { panic(err) }
		assert consumed == enc.len
		assert got.fragment == frag
	}
}
