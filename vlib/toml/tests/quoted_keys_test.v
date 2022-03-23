import toml

fn test_quoted_keys() {
	str_value := 'V rocks!'
	toml_txt := 'a."b.c" = "V rocks!"'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	value := toml_doc.value('a."b.c"')
	assert value == toml.Any(str_value)
	assert value as string == str_value
	assert value.string() == str_value
}
