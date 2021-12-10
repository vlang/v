import toml

fn test_crlf() {
	str_value := 'test string'
	mut toml_txt := 'crlf_string = "test string"\r\n
# Comment with CRLF is not allowed'
	toml_doc := toml.parse(toml_txt) or { panic(err) }

	value := toml_doc.value('crlf_string')
	assert value == toml.Any(str_value)
	assert value as string == str_value
	assert value.string() == str_value
}
