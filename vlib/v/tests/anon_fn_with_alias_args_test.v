type MyString = string
type MyInt = int

struct S1 {
	x fn (a int) MyString
}

struct S2 {
	y fn (a int) MyInt
}

fn get_string(a int) MyString {
	return '${a}'
}

fn get_int(a int) MyInt {
	return a
}

fn test_anon_fn_with_alias_args() {
	s1 := S1{
		x: get_string
	}
	println(s1.x)
	ret1 := s1.x(22)
	println(ret1)
	assert ret1 == '22'

	s2 := S2{
		y: get_int
	}
	println(s2.y)
	ret2 := s2.y(22)
	println(ret2)
	assert ret2 == 22
}
