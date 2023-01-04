interface Str {
	str() string
}

struct St {}

fn (s St) str() string {
	return 's'
}

fn printer(s Str) string {
	println(s)
	return '${s}'
}

fn test_interface_str_method() {
	s := St{}
	ret := printer(s)
	assert ret == 's'
}
