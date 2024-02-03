interface ITest {
	test() bool
}

struct Test {}

fn (p Test) test() bool {
	println('p.test()')
	return true
}

fn get_test(id int) (?ITest, string) {
	match id {
		0 { return Test{}, 'Value 0' }
		else { return none, 'Value not 0' }
	}
}

fn test_main() {
	t, _ := get_test(0)

	if tt := t {
		assert t != none
		assert tt.test() == true
	} else {
		assert false
	}
}
