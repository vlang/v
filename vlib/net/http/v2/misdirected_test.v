module v2

// Tests for 421 Misdirected Request handling (RFC 7540 Section 9.1.2).

fn test_is_misdirected_421() {
	resp := Response{
		status_code: 421
		body:        ''
	}
	assert is_misdirected(resp) == true
}

fn test_is_misdirected_200() {
	resp := Response{
		status_code: 200
		body:        'ok'
	}
	assert is_misdirected(resp) == false
}

fn test_is_misdirected_404() {
	resp := Response{
		status_code: 404
		body:        'not found'
	}
	assert is_misdirected(resp) == false
}

fn test_misdirected_error_message() {
	err := MisdirectedError{
		url:     'https://example.com/path'
		message: '421 Misdirected Request'
	}
	assert err.url == 'https://example.com/path'
	assert err.message == '421 Misdirected Request'
}
