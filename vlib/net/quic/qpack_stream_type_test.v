module quic

fn test_classify_qpack_stream_type() {
	assert classify_qpack_stream_type(0x02)? == .encoder
	assert classify_qpack_stream_type(0x03)? == .decoder
}

fn test_classify_qpack_stream_type_returns_none_for_non_qpack_types() {
	if _ := classify_qpack_stream_type(0x00) {
		assert false, '0x00 is HTTP/3 control, not QPACK'
	}
	if _ := classify_qpack_stream_type(0x01) {
		assert false, '0x01 is HTTP/3 push, not QPACK'
	}
	if _ := classify_qpack_stream_type(0x21) {
		assert false, '0x21 is a grease value, not QPACK'
	}
}

fn test_stream_registry_accepts_first_encoder_and_decoder_stream() {
	mut r := new_qpack_stream_registry()
	r.note_stream_opened(.encoder)!
	r.note_stream_opened(.decoder)!
}

fn test_stream_registry_rejects_second_encoder_stream() {
	mut r := new_qpack_stream_registry()
	r.note_stream_opened(.encoder)!
	if _ := r.note_stream_opened(.encoder) {
		assert false, 'expected an error for a second encoder stream'
	} else {
		assert err.code() == int(H3ErrorCode.stream_creation_error)
	}
}

fn test_stream_registry_rejects_second_decoder_stream() {
	mut r := new_qpack_stream_registry()
	r.note_stream_opened(.decoder)!
	if _ := r.note_stream_opened(.decoder) {
		assert false, 'expected an error for a second decoder stream'
	} else {
		assert err.code() == int(H3ErrorCode.stream_creation_error)
	}
}

fn test_stream_registry_encoder_and_decoder_are_independent() {
	mut r := new_qpack_stream_registry()
	r.note_stream_opened(.encoder)!
	// A second DECODER stream is fine -- only a REPEAT of the same kind errors.
	r.note_stream_opened(.decoder)!
}
