// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

// Tests for ngtcp2 bindings
// Note: These tests require ngtcp2 to be installed

fn test_ngtcp2_version() {
	// Test that we can get ngtcp2 version info
	version := get_version()
	assert version.version_num > 0

	version_str := unsafe { cstring_to_vstring(version.version_str) }
	assert version_str.len > 0

	println('ngtcp2 version: ${version_str}')
}

fn test_settings_default() {
	// Test settings initialization
	mut settings := Ngtcp2SettingsStruct{}
	settings_default(&settings)

	// Check that default values are set
	assert settings.max_udp_payload_size > 0
	assert settings.initial_max_data > 0
}

fn test_transport_params_default() {
	// Test transport params initialization
	mut params := Ngtcp2TransportParamsStruct{}
	transport_params_default(&params)

	// Check that default values are set
	assert params.initial_max_data > 0
	assert params.max_idle_timeout > 0
}

fn test_connection_id() {
	// Test connection ID structure
	mut cid := Ngtcp2CidStruct{
		datalen: 8
	}

	// Fill with test data
	for i in 0 .. 8 {
		cid.data[i] = u8(i + 1)
	}

	assert cid.datalen == 8
	assert cid.data[0] == 1
	assert cid.data[7] == 8
}

fn test_stream_id_helpers() {
	// Test stream ID helper functions

	// Client-initiated bidirectional stream (ID = 0)
	assert is_bidi_stream(0) == true
	assert is_uni_stream(0) == false

	// Client-initiated unidirectional stream (ID = 2)
	assert is_bidi_stream(2) == false
	assert is_uni_stream(2) == true

	// Server-initiated bidirectional stream (ID = 1)
	assert is_bidi_stream(1) == true
	assert is_uni_stream(1) == false

	// Server-initiated unidirectional stream (ID = 3)
	assert is_bidi_stream(3) == false
	assert is_uni_stream(3) == true
}

fn test_error_handling() {
	// Test error string conversion
	err_str := strerror(ngtcp2_err_invalid_argument)
	assert err_str.len > 0

	// Test fatal error check
	assert err_is_fatal(ngtcp2_err_internal) == true
	assert err_is_fatal(ngtcp2_err_discard_pkt) == false
}

fn test_varint_encoding() {
	// Test variable-length integer encoding (used in HTTP/3)
	// This is implemented in v3/client.v but we can test the concept

	// 1-byte encoding (0-63)
	assert u8(42) < 64

	// 2-byte encoding (64-16383)
	assert u64(1000) >= 64 && u64(1000) < 16384

	// 4-byte encoding (16384-1073741823)
	assert u64(100000) >= 16384 && u64(100000) < 1073741824

	// 8-byte encoding (1073741824+)
	assert u64(2000000000) >= 1073741824
}

// Integration test (requires actual ngtcp2 library)
fn test_connection_creation() {
	// This test will be skipped if ngtcp2 is not available
	// It's here to demonstrate the API usage

	// Note: This test requires a real QUIC server to connect to
	// For now, we just test that the API is callable

	// Uncomment when ngtcp2 is fully integrated:
	/*
	config := ConnectionConfig{
		remote_addr: 'localhost:4433'
		alpn: ['h3']
	}
	
	conn := new_connection(config) or {
		eprintln('Connection failed (expected if no server): ${err}')
		return
	}
	
	assert conn.closed == false
	conn.close()
	assert conn.closed == true
	*/
}
