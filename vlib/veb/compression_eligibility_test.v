module veb

import net.http

struct CompressionEligibilityContext {
	Context
}

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
	assert client_accepts_encoding('gzip;\tq=0', 'gzip') == false
	assert client_accepts_encoding('gzip; \tq=0.00\t', 'gzip') == false
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
	assert response_allows_body(205) == false
	assert response_allows_body(304) == false
}

fn test_min_compression_body_bytes_is_viltrum_compatible() {
	assert min_compression_body_bytes == 256
}

fn test_encode_auto_identity_preserves_vary_fields() {
	for offer in ['', 'br', 'gzip;\tq=0, zstd;q=0'] {
		mut ctx := CompressionEligibilityContext{}
		ctx.req.header.set(.accept_encoding, offer)
		ctx.res.body = 'compressible content '.repeat(100)
		ctx.res.header.set(.content_type, 'text/plain')
		ctx.res.header.add(.vary, 'Origin')
		ctx.res.header.add(.vary, 'Accept-Language')
		original := ctx.res.body
		middleware := encode_auto[CompressionEligibilityContext]()
		assert middleware.handler(mut ctx)
		assert ctx.res.body == original
		assert !ctx.res.header.contains(.content_encoding)
		assert ctx.res.header.values(.vary) == ['Origin', 'Accept-Language', 'Accept-Encoding']
	}
}

fn test_compression_preserves_existing_vary_and_avoids_duplicates() {
	for vary in ['Origin', 'Origin, aCcEpT-EnCoDiNg', '*'] {
		mut ctx := Context{}
		ctx.req.header = http.new_header(key: .accept_encoding, value: 'gzip')
		ctx.res.body = 'compressible content '.repeat(100)
		ctx.res.header.set(.content_type, 'text/plain')
		ctx.res.header.set(.vary, vary)
		assert !send_compressed_response(mut ctx, .gzip)
		expected := if vary == 'Origin' { [vary, 'Accept-Encoding'] } else { [vary] }
		assert ctx.res.header.values(.vary) == expected
	}
}

fn test_compression_weakens_identity_etag() {
	for etag in ['"identity"', 'W/"identity"'] {
		mut ctx := Context{}
		ctx.req.header.set(.accept_encoding, 'gzip')
		ctx.res.body = 'compressible content '.repeat(100)
		ctx.res.header.set(.etag, etag)
		assert !send_compressed_response(mut ctx, .gzip)
		assert (ctx.res.header.get(.etag) or { '' }) == 'W/"identity"'
	}
}

fn test_compression_combines_repeated_encoding_fields() {
	for middleware in [encode_auto[CompressionEligibilityContext](),
		encode_gzip[CompressionEligibilityContext](), encode_zstd[CompressionEligibilityContext]()] {
		mut ctx := CompressionEligibilityContext{}
		ctx.req.header.add(.accept_encoding, '*')
		ctx.req.header.add(.accept_encoding, 'gzip;\tq=0, zstd;q=0')
		ctx.res.body = 'compressible content '.repeat(100)
		original := ctx.res.body
		assert middleware.handler(mut ctx)
		assert ctx.res.body == original
		assert !ctx.res.header.contains(.content_encoding)
	}
}

fn test_encode_auto_combines_repeated_fields_before_selecting_encoding() {
	for refused, expected in {
		'zstd;q=0': 'gzip'
		'gzip;q=0': 'zstd'
	} {
		mut ctx := CompressionEligibilityContext{}
		ctx.req.header.add(.accept_encoding, '*')
		ctx.req.header.add(.accept_encoding, refused)
		ctx.res.body = 'compressible content '.repeat(100)
		middleware := encode_auto[CompressionEligibilityContext]()
		assert !middleware.handler(mut ctx)
		assert (ctx.res.header.get(.content_encoding) or { '' }) == expected
	}
}
