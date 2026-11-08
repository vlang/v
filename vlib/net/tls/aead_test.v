module tls

fn test_aead_roundtrip() {
	key := []u8{len: 16, init: 1}
	iv := []u8{len: 12, init: 2}
	pt := [u8(10), 20, 30, 40]
	rec_bytes := seal_record(key, iv, 7, .application_data, pt) or { panic(err) }
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	ct, got := open_record(key, iv, 7, rec) or { panic(err) }
	assert ct == .application_data
	assert got == pt
}

fn test_aead_wrong_seq_fails() {
	key := []u8{len: 16, init: 1}
	iv := []u8{len: 12, init: 2}
	pt := [u8(10), 20, 30]
	rec_bytes := seal_record(key, iv, 7, .application_data, pt) or { panic(err) }
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	open_record(key, iv, 8, rec) or {
		// expected failure
		return
	}
	assert false
}
