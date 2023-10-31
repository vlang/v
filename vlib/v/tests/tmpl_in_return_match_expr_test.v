fn foo1() string {
	return match true {
		true {
			name := 'abc'
			$tmpl('tmpl/template.txt')
		}
		else {
			'else'
		}
	}
}

fn foo2() string {
	return if true {
		name := 'abc'
		$tmpl('tmpl/template.txt')
	} else {
		'else'
	}
}

struct Res {
mut:
	str string
}

fn foo3() Res {
	name := 'abc'
	return Res{
		str: $tmpl('tmpl/template.txt')
	}
}

fn new_res(str string) Res {
	mut res := Res{}
	res.str = str
	return res
}

fn foo4() Res {
	name := 'abc'
	return new_res($tmpl('tmpl/template.txt'))
}

fn test_tmpl_in_return_match_expr() {
	println(foo1())
	println(foo2())
	println(foo3())
	println(foo4())
	assert true
}
