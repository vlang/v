interface IExample {}

struct Example {
	id string
}

struct Example2 {
	n int
}

fn equals(a IExample, b IExample) bool {
	return a == b
}

fn test_equality() {
	assert !equals(Example{ id: 'a' }, 'abc')
	assert !equals(Example{ id: 'a' }, Example{
		id: 'b'
	})
	assert equals(Example{ id: 'a' }, Example{
		id: 'a'
	})
}
