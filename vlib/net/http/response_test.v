module http

fn test_chunk_encode() {
	assert chunk_encode('foobarbaz', 3) == '3\r\nfoo\r\n3\r\nbar\r\n3\r\nbaz\r\n0\r\n\r\n'
}

fn test_bytestr_chunks() {
	resp := Response{
		version: .v1_1
		header: new_header()
		status_code: 200
		text: 'foobarbaz'
	}
	chunks := '6\r\nfoobar\r\n3\r\nbaz\r\n0\r\n\r\n'
	assert resp.bytestr(chunked: true, chunk_size: 6) == 'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n$chunks'
}
