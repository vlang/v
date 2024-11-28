type MyInt = int
type MyOptInt = ?int
type MyOptStr = ?string
type MySumType = f64 | int | string

struct Foo {
	i            int
	i_opt        ?int
	arr          []int
	arr2         []int = [1, 2]
	dec          f64
	dec_opt      ?f64
	dec_arr      []f64
	i_arr        []int
	str_arr      []string
	opt_int      ?int
	opt_int2     ?int = 3
	myalias      MyInt
	myoptlias    ?MyInt
	myoptlias2   ?MyInt = MyInt(1)
	myoptint     MyOptInt
	mysumtype    MySumType
	mysumtypeopt ?MySumType
	str          string
	str2         string = 'b'
	str_opt      ?string
	str_opt2     ?string = 'a'
	str3         MyOptStr
}

struct Bar {
pub:
	foo ?Foo
}

fn test_main() {
	m := Bar{}
	assert m == Bar{}
}
