fn test_main() {
	v := match 12 {
		12 {
			x := if true { 'b' } else { some_func()? }
			assert x == 'b'
			x
		}
		else {
			none
		}
	}
	assert v? == 'b'
}

fn some_func() ?string {
	return 'a'
}
