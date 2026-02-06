struct S1 {
	s1 string = 'abc'
}

struct Empty {
}

type Sum = Empty | S1

fn test_as_cast_already_smartcast_sumtype() {
	a := Sum(S1{})
	if a is S1 {
		println('if expr: ${a.s1}')
		assert a.s1 == 'abc'
		v1 := a as S1
		println('if expr (as cast): ${v1.s1}')
		assert v1.s1 == 'abc'
	}

	match a {
		S1 {
			println('match expr: ${a.s1}')
			assert a.s1 == 'abc'
			v1 := a as S1
			println('match expr (as cast): ${v1.s1}')
			assert v1.s1 == 'abc'
		}
		else {}
	}
}
