interface IExample {}

struct Example {
	n int
}

fn test(a IExample) bool {
	return true
}

fn main() {
	assert test(Example{123})
	assert test(true)
	assert test(123)
	assert test(12.3)
	assert test('abc')
}
