module v2

// Cookie header splitting and joining per RFC 7540 §8.1.2.5.

// split_cookie_headers splits Cookie headers into individual cookie-pair fields
// for better HPACK compression per RFC 7540 §8.1.2.5.
fn split_cookie_headers(headers []HeaderField) []HeaderField {
	mut result := []HeaderField{cap: headers.len}
	for h in headers {
		if h.name != 'cookie' {
			result << h
			continue
		}
		if !h.value.contains('; ') {
			result << h
			continue
		}
		pairs := h.value.split('; ')
		for pair in pairs {
			result << HeaderField{
				name:  'cookie'
				value: pair
			}
		}
	}
	return result
}

// join_cookie_headers concatenates multiple Cookie header fields into a single field
// per RFC 7540 §8.1.2.5.
fn join_cookie_headers(headers []HeaderField) []HeaderField {
	mut cookie_values := []string{cap: 4}
	mut result := []HeaderField{cap: headers.len}
	for h in headers {
		if h.name == 'cookie' {
			cookie_values << h.value
		} else {
			result << h
		}
	}
	if cookie_values.len > 0 {
		result << HeaderField{
			name:  'cookie'
			value: cookie_values.join('; ')
		}
	}
	return result
}
