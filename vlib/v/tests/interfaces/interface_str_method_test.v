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

// for test interface gen to string
interface Abc {}

struct Xyz {}

fn test_interface_gen_to_string() {
	d := Abc(Xyz{})
	mut res := ''
	if d is Xyz {
		println(d)
		res = '${d}'
	}
	assert res == '&Xyz{}'
}
