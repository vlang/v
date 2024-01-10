import toml

fn test_default_to() {
	default_value := 4321
	mut toml_txt := 'var = 1234'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }
	value := toml_doc.value('tar').default_to(default_value).int()
	assert value == default_value
}
