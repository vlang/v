fn test_embed_file_to_string() {
	s := if true {
		$embed_file('./a.txt').to_string()
	} else {
		''
	}
	println(s)
	assert s.trim_space() == 'test'
}
