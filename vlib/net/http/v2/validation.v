module v2

// Header validation for HTTP/2 requests per RFC 7540 §8.1.2.

// known_pseudo_headers lists the valid HTTP/2 request pseudo-headers per RFC 7540 §8.1.2.3.
const known_pseudo_headers = [':method', ':path', ':scheme', ':authority']

// known_response_pseudo_headers lists the valid HTTP/2 response pseudo-headers per RFC 7540 §8.1.2.1.
const known_response_pseudo_headers = [':status']

// forbidden_headers lists connection-specific headers forbidden in HTTP/2 per RFC 7540 §8.1.2.2.
const forbidden_headers = ['connection', 'keep-alive', 'proxy-connection', 'upgrade']

// validate_request_headers validates HTTP/2 request headers per RFC 7540 §8.1.2.
// Checks pseudo-header presence, ordering, and forbidden connection-specific headers.
// CONNECT requests require only :method and :authority (RFC 7540 §8.3).
pub fn validate_request_headers(headers []HeaderField) ! {
	mut has_method := false
	mut has_path := false
	mut has_authority := false
	mut is_connect := false
	mut pseudo_ended := false

	for h in headers {
		if h.name.starts_with(':') {
			if pseudo_ended {
				return error('PROTOCOL_ERROR: pseudo-header ${h.name} after regular header')
			}
			if h.name !in known_pseudo_headers {
				return error('PROTOCOL_ERROR: unknown pseudo-header ${h.name}')
			}
			if h.name == ':method' {
				has_method = true
				is_connect = h.value == 'CONNECT'
			} else if h.name == ':path' {
				has_path = true
			} else if h.name == ':authority' {
				has_authority = true
			}
		} else {
			pseudo_ended = true
			check_forbidden_header(h)!
		}
	}

	if !has_method {
		return error('PROTOCOL_ERROR: missing required :method pseudo-header')
	}
	// RFC 7540 §8.3: CONNECT requires :authority, not :path or :scheme
	if is_connect {
		if !has_authority {
			return error('PROTOCOL_ERROR: CONNECT requires :authority pseudo-header (RFC 7540 §8.3)')
		}
	} else if !has_path {
		return error('PROTOCOL_ERROR: missing required :path pseudo-header')
	}
}

// check_forbidden_header rejects a single header if it is connection-specific
// and forbidden in HTTP/2 per RFC 7540 §8.1.2.2.
fn check_forbidden_header(h HeaderField) ! {
	lower := h.name.to_lower()
	if lower in forbidden_headers {
		return error('PROTOCOL_ERROR: forbidden connection-specific header: ${lower}')
	}
	if lower == 'transfer-encoding' && h.value.to_lower() != 'trailers' {
		return error('PROTOCOL_ERROR: forbidden transfer-encoding value: ${h.value} (only trailers allowed)')
	}
}

// filter_connection_specific_headers removes HTTP/1.1 connection-specific headers
// that are forbidden in HTTP/2 per RFC 7540 §8.1.2.2.
// transfer-encoding: trailers is the only allowed exception.
pub fn filter_connection_specific_headers(headers map[string]string) map[string]string {
	mut result := map[string]string{}
	for key, value in headers {
		lower := key.to_lower()
		if lower in forbidden_headers {
			continue
		}
		if lower == 'transfer-encoding' && value.to_lower() != 'trailers' {
			continue
		}
		result[key] = value
	}
	return result
}

// validate_response_headers validates HTTP/2 response headers per RFC 7540 §8.1.2.1.
// Checks that :status is present, no request pseudo-headers appear, and
// pseudo-headers come before regular headers.
pub fn validate_response_headers(headers []HeaderField) ! {
	mut has_status := false
	mut pseudo_ended := false

	for h in headers {
		if h.name.starts_with(':') {
			if pseudo_ended {
				return error('PROTOCOL_ERROR: pseudo-header ${h.name} after regular header')
			}
			if h.name in known_pseudo_headers {
				return error('PROTOCOL_ERROR: request pseudo-header ${h.name} in response')
			}
			if h.name !in known_response_pseudo_headers {
				return error('PROTOCOL_ERROR: unknown response pseudo-header ${h.name}')
			}
			if h.name == ':status' {
				has_status = true
			}
		} else {
			pseudo_ended = true
		}
	}

	if !has_status {
		return error('PROTOCOL_ERROR: missing required :status pseudo-header')
	}
}
