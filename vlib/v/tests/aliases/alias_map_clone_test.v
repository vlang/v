type Fields = map[string]string

fn test_alias_map_clone() {
	f := Fields({
		's': 'a'
	})

	s := f.clone()
	println(s)
	assert s == {
		's': 'a'
	}
}
