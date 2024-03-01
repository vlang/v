type Name[T] = T | int | string

fn f(n Name[string]) {
	assert n as string == 'lolol'
}

fn g(n Name[int]) {
	assert n as int == 123
}

fn test_main() {
	f(Name[string]('lolol'))
	g(Name[int](123))
}
