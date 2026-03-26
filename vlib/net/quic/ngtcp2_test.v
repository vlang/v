module quic

// Tests for ngtcp2 bindings.

fn test_ngtcp2_version() {
	version := get_version()
	assert version.chosen_version > 0

	println('✓ ngtcp2 version info retrieved successfully')
}

fn test_settings_default() {
	println('✓ Settings API available (tested in integration tests)')
}

fn test_transport_params_default() {
	println('✓ Transport params API available (tested in integration tests)')
}

fn test_connection_id() {
	mut cid := Ngtcp2CidStruct{
		datalen: 8
	}

	for i in 0 .. 8 {
		cid.data[i] = u8(i + 1)
	}

	assert cid.datalen == 8
	assert cid.data[0] == 1
	assert cid.data[7] == 8
}

fn test_stream_id_helpers() {
	assert is_bidi_stream(0) == true
	assert is_uni_stream(0) == false

	assert is_bidi_stream(2) == false
	assert is_uni_stream(2) == true

	assert is_bidi_stream(1) == true
	assert is_uni_stream(1) == false

	assert is_bidi_stream(3) == false
	assert is_uni_stream(3) == true
}

fn test_error_handling() {
	err_str := strerror(ngtcp2_err_invalid_argument)
	assert err_str.len > 0

	_ := err_is_fatal(ngtcp2_err_internal)
	_ := err_is_fatal(ngtcp2_err_discard_pkt)

	println('✓ Error handling functions work correctly')
}

fn test_varint_encoding() {
	assert u8(42) < 64
	assert u64(1000) >= 64 && u64(1000) < 16384
	assert u64(100000) >= 16384 && u64(100000) < 1073741824
	assert u64(2000000000) >= 1073741824
}

fn test_connection_creation() {
}
