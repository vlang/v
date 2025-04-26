import toml

fn test_quoted_string_crlf() {
	toml_txt := 'str1 = """tcc \\\r\nabc \\\r\n123"""'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	dump(toml_doc)
	value := toml_doc.value('str1').string()
	assert value == 'tcc abc 123'
}
