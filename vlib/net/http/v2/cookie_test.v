module v2

// Tests for RFC 7540 §8.1.2.5 Cookie header splitting and joining.

fn test_split_cookie_single_pair_unchanged() {
	headers := [HeaderField{
		name:  'cookie'
		value: 'a=1'
	}]
	result := split_cookie_headers(headers)
	assert result.len == 1
	assert result[0].name == 'cookie'
	assert result[0].value == 'a=1'
}

fn test_split_cookie_multiple_pairs() {
	headers := [HeaderField{
		name:  'cookie'
		value: 'a=1; b=2'
	}]
	result := split_cookie_headers(headers)
	assert result.len == 2
	assert result[0].name == 'cookie'
	assert result[0].value == 'a=1'
	assert result[1].name == 'cookie'
	assert result[1].value == 'b=2'
}

fn test_split_cookie_three_pairs() {
	headers := [HeaderField{
		name:  'cookie'
		value: 'a=1; b=2; c=3'
	}]
	result := split_cookie_headers(headers)
	assert result.len == 3
	assert result[0].value == 'a=1'
	assert result[1].value == 'b=2'
	assert result[2].value == 'c=3'
}

fn test_split_cookie_non_cookie_headers_pass_through() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'cookie'
			value: 'a=1; b=2'
		},
		HeaderField{
			name:  'accept'
			value: 'text/html'
		},
	]
	result := split_cookie_headers(headers)
	assert result.len == 4
	assert result[0].name == ':method'
	assert result[0].value == 'GET'
	assert result[1].name == 'cookie'
	assert result[1].value == 'a=1'
	assert result[2].name == 'cookie'
	assert result[2].value == 'b=2'
	assert result[3].name == 'accept'
	assert result[3].value == 'text/html'
}

fn test_split_cookie_no_cookies() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	result := split_cookie_headers(headers)
	assert result.len == 2
	assert result[0].name == ':method'
	assert result[1].name == ':path'
}

fn test_split_cookie_special_characters_preserved() {
	headers := [
		HeaderField{
			name:  'cookie'
			value: 'token=abc+def/ghi=; session=xyz%3D123'
		},
	]
	result := split_cookie_headers(headers)
	assert result.len == 2
	assert result[0].value == 'token=abc+def/ghi='
	assert result[1].value == 'session=xyz%3D123'
}

fn test_join_cookie_multiple_headers() {
	headers := [
		HeaderField{
			name:  'cookie'
			value: 'a=1'
		},
		HeaderField{
			name:  'cookie'
			value: 'b=2'
		},
		HeaderField{
			name:  'cookie'
			value: 'c=3'
		},
	]
	result := join_cookie_headers(headers)
	assert result.len == 1
	assert result[0].name == 'cookie'
	assert result[0].value == 'a=1; b=2; c=3'
}

fn test_join_cookie_single_header_unchanged() {
	headers := [HeaderField{
		name:  'cookie'
		value: 'a=1'
	}]
	result := join_cookie_headers(headers)
	assert result.len == 1
	assert result[0].name == 'cookie'
	assert result[0].value == 'a=1'
}

fn test_join_cookie_no_cookies() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	result := join_cookie_headers(headers)
	assert result.len == 2
	assert result[0].name == ':method'
	assert result[1].name == ':path'
}

fn test_join_cookie_preserves_non_cookie_order() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'cookie'
			value: 'a=1'
		},
		HeaderField{
			name:  'accept'
			value: 'text/html'
		},
		HeaderField{
			name:  'cookie'
			value: 'b=2'
		},
	]
	result := join_cookie_headers(headers)
	assert result.len == 3
	assert result[0].name == ':method'
	assert result[1].name == 'accept'
	assert result[2].name == 'cookie'
	assert result[2].value == 'a=1; b=2'
}

fn test_roundtrip_split_then_join() {
	original := [HeaderField{
		name:  'cookie'
		value: 'a=1; b=2; c=3'
	}]
	split := split_cookie_headers(original)
	assert split.len == 3
	joined := join_cookie_headers(split)
	assert joined.len == 1
	assert joined[0].name == 'cookie'
	assert joined[0].value == 'a=1; b=2; c=3'
}

fn test_roundtrip_with_mixed_headers() {
	original := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'cookie'
			value: 'session=abc; token=xyz'
		},
		HeaderField{
			name:  'accept'
			value: '*/*'
		},
	]
	split := split_cookie_headers(original)
	joined := join_cookie_headers(split)
	// Non-cookie headers preserved, cookie rejoined
	mut has_method := false
	mut has_accept := false
	mut cookie_val := ''
	for h in joined {
		if h.name == ':method' {
			has_method = true
		}
		if h.name == 'accept' {
			has_accept = true
		}
		if h.name == 'cookie' {
			cookie_val = h.value
		}
	}
	assert has_method
	assert has_accept
	assert cookie_val == 'session=abc; token=xyz'
}

fn test_split_cookie_empty_value() {
	headers := [HeaderField{
		name:  'cookie'
		value: ''
	}]
	result := split_cookie_headers(headers)
	assert result.len == 1
	assert result[0].name == 'cookie'
	assert result[0].value == ''
}
