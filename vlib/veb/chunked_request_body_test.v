module veb

$if !new_veb ? {
	fn test_decode_chunked_request_body() {
		encoded := '4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n'
		decoded := decode_chunked_request_body(encoded) or { panic(err) }
		assert decoded == 'Wikipedia'
	}

	fn test_decode_chunked_request_body_with_extensions_and_trailers() {
		encoded := '4;foo=bar\r\nWiki\r\n0\r\nX-Trace: true\r\n\r\n'
		decoded := decode_chunked_request_body(encoded) or { panic(err) }
		assert decoded == 'Wiki'
	}

	fn test_decode_chunked_request_body_with_incomplete_data() {
		encoded := '4\r\nWiki\r\n5\r\nped'
		decode_chunked_request_body(encoded) or {
			assert err is IncompleteChunkedRequestBodyError
			return
		}
		assert false
	}

	fn test_decode_chunked_request_body_with_invalid_data() {
		encoded := 'z\r\nWiki\r\n0\r\n\r\n'
		decode_chunked_request_body(encoded) or {
			assert err.msg() == 'invalid chunk size line'
			return
		}
		assert false
	}
}
