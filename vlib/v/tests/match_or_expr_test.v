struct Foo {
	option ?int = none
}

fn test_main() {
	test := true
	foo := Foo{}
	result := foo.option or {
		match test {
			true { 1 }
			else { 2 }
		}
	}

	assert result == 1
}
