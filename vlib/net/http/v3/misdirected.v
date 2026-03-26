module v3

// 421 Misdirected Request handling per RFC 9114.

// MisdirectedError represents a 421 Misdirected Request response.
pub struct MisdirectedError {
pub:
	url     string
	message string
}

// is_misdirected returns true if the response has a 421 status code
// per RFC 9114.
pub fn is_misdirected(response Response) bool {
	return response.status_code == 421
}

// handle_misdirected retries a request on a fresh connection when a 421 is received.
// Only retries once to prevent infinite loops. Returns the retry response
// or an error if the retry also fails.
pub fn handle_misdirected(address string, req Request) !Response {
	mut fresh_client := new_client(address)!

	defer {
		fresh_client.close()
	}

	return fresh_client.request(req) or { return error('misdirected retry failed: ${err}') }
}
