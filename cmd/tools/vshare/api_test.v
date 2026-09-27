module vshare

fn test_share_url_from_response_accepts_successful_response() {
	url := share_url_from_response(200, '{"hash":"21cf286fdb","error":""}')!
	assert url == 'https://play.vlang.io/p/21cf286fdb'
}

fn test_share_url_from_response_reports_http_errors() {
	if url := share_url_from_response(502, 'error code: 502') {
		assert false, 'expected an error, got ${url}'
	} else {
		assert err.msg() == 'Failed to share code: playground returned HTTP 502: error code: 502'
	}
}

fn test_share_url_from_response_reports_malformed_json() {
	if url := share_url_from_response(200, 'error code: 502') {
		assert false, 'expected an error, got ${url}'
	} else {
		assert err.msg().starts_with('Failed to decode playground response:')
	}
}

fn test_share_url_from_response_reports_api_errors() {
	if url := share_url_from_response(200, '{"hash":"","error":"snippet was not saved"}') {
		assert false, 'expected an error, got ${url}'
	} else {
		assert err.msg() == 'Failed to share code: snippet was not saved'
	}
}

fn test_share_url_from_response_requires_hash() {
	if url := share_url_from_response(200, '{"hash":"","error":""}') {
		assert false, 'expected an error, got ${url}'
	} else {
		assert err.msg() == 'Failed to share code: playground response did not include a hash'
	}
}
