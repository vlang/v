const self_file = $embed_file(@FILE)

fn test_self_file() {
	source := self_file.to_string()
	assert source.contains('self_file.to_string')
	assert source.contains('fn test_self_file() {')
	assert source.split_into_lines().len > @LINE.int()
}
