module v3

// HTTP/3 request header validation per RFC 9114 §4.1.2.

// h3_known_pseudo_headers lists valid HTTP/3 request pseudo-headers.
// Includes :protocol for extended CONNECT per RFC 9220.
const h3_known_pseudo_headers = [':method', ':path', ':scheme', ':authority', ':protocol']

// h3_forbidden_headers lists connection-specific headers forbidden in HTTP/3
// per RFC 9114 §4.2.
const h3_forbidden_headers = ['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding',
	'upgrade']

// validate_h3_request_headers validates HTTP/3 request headers per RFC 9114 §4.1.2.
// Checks pseudo-header presence, ordering, and forbidden connection-specific headers.
pub fn validate_h3_request_headers(headers []HeaderField) ! {
	mut has_method := false
	mut has_path := false
	mut has_authority := false
	mut is_connect := false
	mut pseudo_ended := false

	for h in headers {
		if h.name.starts_with(':') {
			if pseudo_ended {
				return error('H3_MESSAGE_ERROR: pseudo-header ${h.name} after regular header (RFC 9114 §4.1.2)')
			}
			if h.name !in h3_known_pseudo_headers {
				return error('H3_MESSAGE_ERROR: unknown pseudo-header ${h.name} (RFC 9114 §4.1.2)')
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
			check_h3_forbidden_header(h)!
		}
	}

	if !has_method {
		return error('H3_MESSAGE_ERROR: missing required :method pseudo-header (RFC 9114 §4.1.2)')
	}
	// RFC 9114 §4.4: CONNECT requires :authority, not :path or :scheme
	if is_connect {
		if !has_authority {
			return error('H3_MESSAGE_ERROR: CONNECT requires :authority pseudo-header (RFC 9114 §4.4)')
		}
	} else if !has_path {
		return error('H3_MESSAGE_ERROR: missing required :path pseudo-header (RFC 9114 §4.1.2)')
	}
}

// check_h3_forbidden_header rejects a single header if it is connection-specific
// and forbidden in HTTP/3 per RFC 9114 §4.2.
fn check_h3_forbidden_header(h HeaderField) ! {
	lower := h.name.to_lower()
	if lower in h3_forbidden_headers {
		return error('H3_MESSAGE_ERROR: forbidden connection-specific header: ${lower} (RFC 9114 §4.2)')
	}
}
