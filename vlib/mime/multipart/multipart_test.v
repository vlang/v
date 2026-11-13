module multipart

fn test_parse_fields_and_files() {
	body := '--b\r\nContent-Disposition: form-data; name="field1"\r\n\r\nvalue1\r\n--b\r\nContent-Disposition: form-data; name="file1"; filename="a.txt"\r\nContent-Type: text/plain\r\n\r\nfiledata\r\n--b--\r\n'
	parts := parse(body, 'b')
	assert parts.len == 2
	mut found_field := false
	mut found_file := false
	for p in parts {
		if p.field_name == 'field1' {
			assert p.data == 'value1'
			found_field = true
		}
		if p.field_name == 'file1' {
			assert p.file_name == 'a.txt'
			assert p.content_type == 'text/plain'
			assert p.data == 'filedata'
			found_file = true
		}
	}
	assert found_field
	assert found_file
}

fn test_parse_empty() {
	parts := parse('', 'b')
	assert parts.len == 0
}

fn test_parse_preserves_duplicate_names_and_wire_order() {
	body := '--b\r\nContent-Disposition: form-data; name="x"\r\n\r\none\r\n--b\r\nContent-Disposition: form-data; name="f"; filename="a"\r\nContent-Type: text/plain\r\n\r\nfile\r\n--b\r\nContent-Disposition: form-data; name="x"\r\n\r\ntwo\r\n--b--\r\n'
	parts := parse(body, 'b')
	assert parts.len == 3
	assert parts[0].field_name == 'x'
	assert parts[0].data == 'one'
	assert parts[1].file_name == 'a'
	assert parts[2].field_name == 'x'
	assert parts[2].data == 'two'
}

fn test_boundary_text_inside_body_is_data() {
	payload := 'before b after\r\n--b-extra\r\n\x00\xff\r\n\r\n'
	body := '--b\r\nContent-Disposition: form-data; name="x"\r\n\r\n${payload}\r\n--b--\r\n'
	parts := parse(body, 'b')
	assert parts.len == 1
	assert parts[0].data == payload
}

fn test_header_order_and_optional_content_type() {
	body := '--b\r\nContent-Type: text/plain; charset=utf-8\r\nX-Extra: ignored\r\ncontent-disposition: form-data; name="text"\r\n\r\nhello\r\n--b\r\nContent-Disposition: form-data; name="file"; filename="empty.txt"\r\n\r\n\r\n--b--'
	parts := parse(body, 'b')
	assert parts.len == 2
	assert parts[0].field_name == 'text'
	assert parts[0].content_type == 'text/plain; charset=utf-8'
	assert parts[0].data == 'hello'
	assert parts[1].file_name == 'empty.txt'
	assert parts[1].content_type == ''
	assert parts[1].data == ''
}

fn test_quoted_disposition_parameters() {
	body := '--b\r\nContent-Disposition: form-data; name="a;b"; filename="a;\\"b.txt"\r\nContent-Type: text/plain\r\n\r\nx\r\n--b--\r\n'
	parts := parse(body, 'b')
	assert parts.len == 1
	assert parts[0].field_name == 'a;b'
	assert parts[0].file_name == 'a;"b.txt'
}

fn test_preamble_epilogue_and_boundary_padding() {
	body := 'preamble with b\r\n--b \t\r\nContent-Disposition: form-data; name=x\r\n\r\nvalue\r\n--b--\t \r\nepilogue\r\n--b\r\n'
	parts := parse(body, 'b')
	assert parts.len == 1
	assert parts[0].data == 'value'
}

fn test_malformed_and_truncated_input() {
	for boundary in ['', 'b\r\nx', 'b\n', 'b ', 'b'.repeat(71)] {
		assert parse('--b--\r\n', boundary).len == 0
	}
	for body in ['', 'b', '--b', '--b\r\n', '--b\r\nBroken header\r\n\r\nx\r\n--b--',
		'--b\r\nContent-Disposition: form-data; name="x"\r\n\r\nx'] {
		assert parse(body, 'b').len == 0
	}
	body := '--b\r\nContent-Disposition: form-data; name="ok"\r\n\r\none\r\n--b\r\nContent-Disposition: form-data; name="unfinished"\r\n\r\ntwo'
	parts := parse(body, 'b')
	assert parts.len == 1
	assert parts[0].field_name == 'ok'
}

fn test_headerless_parts_and_folded_metadata() {
	body := '--b\r\n\r\nraw\r\n--b\r\nContent-Disposition: attachment;\r\n\tfilename="x.txt"\r\nContent-Type: text/plain;\r\n charset=utf-8\r\n\r\nfile\r\n--b--\r\n'
	parts := parse(body, 'b')
	assert parts.len == 2
	assert parts[0].field_name == ''
	assert parts[0].content_type == ''
	assert parts[0].data == 'raw'
	assert parts[1].file_name == 'x.txt'
	assert parts[1].content_type == 'text/plain; charset=utf-8'
	assert parts[1].data == 'file'
}

fn test_invalid_headers_stop_after_complete_parts() {
	for header in ['Bad Header: x', 'X-Test: \x00', 'X-Test: \x01', '\tno preceding header',
		'Content-Disposition: form-data; name="unterminated',
		'Content-Disposition: form-data; name=x; NAME=y',
		'Content-Type: text/plain\r\nContent-Type: text/html'] {
		body := '--b\r\n\r\nfirst\r\n--b\r\n${header}\r\n\r\nsecond\r\n--b\r\n\r\nthird\r\n--b--\r\n'
		parts := parse(body, 'b')
		assert parts.len == 1
		assert parts[0].data == 'first'
	}
}
