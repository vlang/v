fn test_for_in_mut_val_with_if_val_in() {
	array := ['e']
	mut splited := ['h', 'f', 's', 'e']
	mut has_e := false

	for mut s in splited {
		if s in array {
			println('Hello')
			has_e = true
		}
	}
	assert has_e
}
