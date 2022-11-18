import toml

fn test_crlf() {
	str_value := 'test string'
	mut toml_txt := 'crlf_string = "test string"\r\n
# Comment with CRLF is not allowed'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('crlf_string')
	assert value == toml.Any(str_value)
	assert value as string == str_value
	assert value.string() == str_value
}

fn test_crlf_is_parsable_just_like_lf() {
	crlf_content := '# a comment\r\ntitle = "TOML Example"\r\n[database]\r\nserver = "192.168.1.1"\r\nports = [ 8000, 8001, 8002 ]\r\n'
	all := [crlf_content, crlf_content.replace('\r\n', '\n')]
	for content in all {
		res := toml.parse_text(content)?
		assert res.value('title') == toml.Any('TOML Example')
		assert (res.value('database') as map[string]toml.Any)['server']! == toml.Any('192.168.1.1')
	}
}
