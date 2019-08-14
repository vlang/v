struct Test {
}

struct Generic<T> {
	test string
}

pub fn test_generic() {
	generic := create_generic<Test>()
}

pub fn create_generic<T>() Generic<T> {
	return Generic<Test>{'generics'}
}

pub fn (g Generic<T>) hello() {
	println(g.text)
	say_hi<T>()
}

pub fn say_hi<T>() {
	println('hi')
}