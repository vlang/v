struct User {}

fn User.is_ok() bool {
	return true
}

fn test_main() {
	a := match true {
		User.is_ok() {
			1
		}
		else {
			2
		}
	}
	assert a == 1
}
