module brotli

fn test_decompress_rejects_missing_stream() {
	if _ := decompress([]u8{}) {
		assert false, 'empty input is not an encoded empty stream'
	} else {
		assert err.msg() == 'brotli: empty input'
	}
	if _ := decompress([]u8{}, concatenated: true) {
		assert false, 'concatenated decoding requires an encoded stream'
	} else {
		assert err.msg() == 'brotli: empty input'
	}
}

fn test_decompress_rejects_reserved_window_marker() {
	if _ := decompress([u8(0x91), 0x03]) {
		assert false, 'a reserved window marker must not decode as an empty stream'
	} else {
		assert err.msg() == 'brotli: invalid large window marker'
	}
}

fn test_decompress_accepts_encoded_empty_streams() {
	assert decompress([u8(6)])! == []u8{}
	assert decompress([u8(6), 6], concatenated: true)! == []u8{}
	encoded := compress('payload'.bytes())!
	mut streams := [u8(6)]
	streams << encoded
	streams << u8(6)
	assert decompress(streams, concatenated: true)! == 'payload'.bytes()
}
