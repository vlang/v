module main

struct TestA {
	aa int
}

fn test_main() {
	func := &fn () int {
		arr := []TestA{}
		return arr.len
	}
	assert func() == 0

	func2 := &fn () int {
		arr := []TestA{}
		return arr.len
	}
	assert func2() == 0
}
