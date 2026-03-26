module v2

// Tests for h2c Upgrade from HTTP/1.1 per RFC 7540 §3.2.
import encoding.base64

fn test_detect_h2c_upgrade_valid() {
	// Build a valid HTTP/1.1 upgrade request with HTTP2-Settings
	settings_payload := encode_settings_payload(Settings{
		max_concurrent_streams: 100
		initial_window_size:    65535
	})
	encoded_settings := base64.url_encode(settings_payload)

	request := 'GET / HTTP/1.1\r\n' + 'Host: example.com\r\n' +
		'Connection: Upgrade, HTTP2-Settings\r\n' + 'Upgrade: h2c\r\n' +
		'HTTP2-Settings: ${encoded_settings}\r\n' + '\r\n'

	result := detect_h2c_upgrade(request.bytes()) or {
		assert false, 'expected upgrade to be detected, got none'
		return
	}

	assert result.method == 'GET'
	assert result.path == '/'
	assert result.headers['host'] == 'example.com'
	assert result.h2_settings.len > 0
	assert result.h2_settings == settings_payload
}

fn test_detect_h2c_upgrade_no_upgrade() {
	// Regular HTTP/1.1 request without upgrade headers
	request := 'GET / HTTP/1.1\r\nHost: example.com\r\n\r\n'

	if _ := detect_h2c_upgrade(request.bytes()) {
		assert false, 'expected none for non-upgrade request'
	}
}

fn test_detect_h2c_upgrade_missing_settings() {
	// Upgrade: h2c but missing HTTP2-Settings header
	request := 'GET / HTTP/1.1\r\n' + 'Host: example.com\r\n' +
		'Connection: Upgrade, HTTP2-Settings\r\n' + 'Upgrade: h2c\r\n' + '\r\n'

	if _ := detect_h2c_upgrade(request.bytes()) {
		assert false, 'expected none when HTTP2-Settings is missing'
	}
}

fn test_build_upgrade_request() {
	settings := Settings{
		max_concurrent_streams: 100
		initial_window_size:    65535
	}

	result := build_upgrade_request('GET', '/', 'example.com', settings)

	// Verify request line
	assert result.starts_with('GET / HTTP/1.1\r\n')

	// Verify required headers present
	assert result.contains('Host: example.com\r\n')
	assert result.contains('Connection: Upgrade, HTTP2-Settings\r\n')
	assert result.contains('Upgrade: h2c\r\n')
	assert result.contains('HTTP2-Settings: ')

	// Verify ends with double CRLF
	assert result.ends_with('\r\n\r\n')

	// Verify the HTTP2-Settings value is valid base64url-encoded settings
	mut settings_value := ''
	for line in result.split('\r\n') {
		if line.starts_with('HTTP2-Settings: ') {
			settings_value = line.all_after('HTTP2-Settings: ')
			break
		}
	}
	assert settings_value.len > 0
	decoded := base64.url_decode(settings_value)
	assert decoded.len % 6 == 0
}

fn test_apply_upgrade_settings() {
	// Encode known settings into a payload
	settings_payload := encode_settings_payload(Settings{
		header_table_size:      8192
		max_concurrent_streams: 200
		initial_window_size:    32768
		max_frame_size:         32768
	})

	result := apply_upgrade_settings(settings_payload) or {
		assert false, 'apply_upgrade_settings failed: ${err}'
		return
	}

	assert result.header_table_size == 8192
	assert result.max_concurrent_streams == 200
	assert result.initial_window_size == 32768
	assert result.max_frame_size == 32768
}

fn test_apply_upgrade_settings_empty() {
	// Empty payload should return default settings
	result := apply_upgrade_settings([]u8{}) or {
		assert false, 'apply_upgrade_settings failed on empty: ${err}'
		return
	}

	assert result.header_table_size == 4096
	assert result.initial_window_size == 65535
}

fn test_apply_upgrade_settings_invalid() {
	// Payload not a multiple of 6 bytes should error
	apply_upgrade_settings([u8(0), 1, 2]) or {
		assert err.msg().contains('invalid')
		return
	}
	assert false, 'expected error for invalid settings payload'
}

fn test_send_101_response() {
	mut buf := TestBuffer{}
	mut conn := ServerConn(&buf)

	send_101_response(mut conn) or {
		assert false, 'send_101_response failed: ${err}'
		return
	}

	response := buf.written_data.bytestr()
	assert response == 'HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: h2c\r\n\r\n'
}

fn test_detect_h2c_upgrade_post_with_body_indication() {
	settings_payload := encode_settings_payload(Settings{})
	encoded_settings := base64.url_encode(settings_payload)

	request := 'POST /submit HTTP/1.1\r\n' + 'Host: example.com\r\n' +
		'Connection: Upgrade, HTTP2-Settings\r\n' + 'Upgrade: h2c\r\n' +
		'HTTP2-Settings: ${encoded_settings}\r\n' + 'Content-Length: 5\r\n' + '\r\n'

	result := detect_h2c_upgrade(request.bytes()) or {
		assert false, 'expected upgrade to be detected for POST'
		return
	}

	assert result.method == 'POST'
	assert result.path == '/submit'
	assert result.headers['content-length'] == '5'
}

// TestBuffer is a mock ServerConn that captures written data.
struct TestBuffer {
mut:
	written_data []u8
	read_data    []u8
	read_pos     int
}

fn (mut b TestBuffer) read(mut buf []u8) !int {
	if b.read_pos >= b.read_data.len {
		return error('EOF')
	}
	n := if b.read_pos + buf.len > b.read_data.len {
		b.read_data.len - b.read_pos
	} else {
		buf.len
	}
	for i in 0 .. n {
		buf[i] = b.read_data[b.read_pos + i]
	}
	b.read_pos += n
	return n
}

fn (mut b TestBuffer) write(data []u8) !int {
	b.written_data << data
	return data.len
}

fn (mut b TestBuffer) close() ! {
}
