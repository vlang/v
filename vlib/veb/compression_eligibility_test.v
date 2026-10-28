module veb

// Pure unit tests for the compression eligibility helpers in middleware.v.
// No network involved.
fn test_client_accepts_encoding_plain() {
	assert client_accepts_encoding('gzip', 'gzip') == true
	assert client_accepts_encoding('gzip, zstd, br', 'zstd') == true
	assert client_accepts_encoding('gzip, zstd, br', 'gzip') == true
}

fn test_client_accepts_encoding_no_match() {
	assert client_accepts_encoding('br', 'gzip') == false
	assert client_accepts_encoding('br', 'zstd') == false
	assert client_accepts_encoding('', 'gzip') == false
}

fn test_client_accepts_encoding_q_zero_refuses() {
	assert client_accepts_encoding('gzip;q=0', 'gzip') == false
	assert client_accepts_encoding('gzip; q=0', 'gzip') == false
	assert client_accepts_encoding('gzip;q=0.00', 'gzip') == false
	assert client_accepts_encoding('gzip;q=0, zstd', 'gzip') == false
	assert client_accepts_encoding('gzip;q=0, zstd', 'zstd') == true
}

fn test_client_accepts_encoding_positive_q_accepts() {
	assert client_accepts_encoding('gzip;q=0.5', 'gzip') == true
	assert client_accepts_encoding('gzip;q=1', 'gzip') == true
}

fn test_client_accepts_encoding_wildcard() {
	assert client_accepts_encoding('*', 'gzip') == true
	assert client_accepts_encoding('*;q=0.8', 'zstd') == true
	assert client_accepts_encoding('*;q=0', 'gzip') == false
	// An explicit entry wins over `*`.
	assert client_accepts_encoding('gzip;q=0, *;q=1', 'gzip') == false
	assert client_accepts_encoding('*;q=1, gzip;q=0', 'gzip') == false
	assert client_accepts_encoding('br, *;q=1', 'gzip') == true
}

fn test_client_accepts_encoding_case_insensitive() {
	assert client_accepts_encoding('GZip', 'gzip') == true
	assert client_accepts_encoding('ZSTD', 'zstd') == true
	assert client_accepts_encoding('  gzip  ', 'gzip') == true
}

fn test_compressible_content_type() {
	assert compressible_content_type('text/plain') == true
	assert compressible_content_type('text/html; charset=utf-8') == true
	assert compressible_content_type('application/json') == true
	assert compressible_content_type('Application/JSON; charset=utf-8') == true
	assert compressible_content_type('application/javascript') == true
	assert compressible_content_type('image/svg+xml') == true
	assert compressible_content_type('application/xml') == true
	// Untyped responses keep the previous behavior.
	assert compressible_content_type('') == true
}

fn test_incompressible_content_type() {
	assert compressible_content_type('image/png') == false
	assert compressible_content_type('image/jpeg') == false
	assert compressible_content_type('application/octet-stream') == false
	assert compressible_content_type('application/zip') == false
	assert compressible_content_type('application/pdf') == false
	assert compressible_content_type('video/mp4') == false
}

fn test_response_allows_body() {
	assert response_allows_body(200) == true
	assert response_allows_body(404) == true
	assert response_allows_body(500) == true
	assert response_allows_body(101) == false
	assert response_allows_body(204) == false
	assert response_allows_body(304) == false
}

fn test_min_compression_body_bytes_is_viltrum_compatible() {
	assert min_compression_body_bytes == 256
}
