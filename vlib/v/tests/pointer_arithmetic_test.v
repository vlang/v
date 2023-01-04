fn test_strings() {
	s := 'hi'
	mut p := unsafe { s.str + 1 }
	n := unsafe { p - s.str }
	assert typeof(n).name == 'int'
}
