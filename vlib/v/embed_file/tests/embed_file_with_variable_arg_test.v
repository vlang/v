fn test_embed_file_with_variable_arg() {
	path := './a.txt'
	s := $embed_file(path).to_string()
	println(s)
	assert s.trim_space() == 'test'
}
