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
