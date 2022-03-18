interface Any {}

fn print_out(x Any) string {
	if x is string {
		println(x)
		return '$x'
	}
	return ''
}

fn test_empty_interface() {
	ret := print_out('12345')
	assert ret == '&12345'
}
