fn foo[T](v &T) string {
	return typeof(v).name
}

fn test_main() {
	i := i8(0)
	ip := &i
	ipp := &ip
	assert foo(ip) == '&i8'
	assert foo(ipp) == '&&i8'
}
