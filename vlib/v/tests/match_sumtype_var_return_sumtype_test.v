module main

type Sum = Struct | int

struct Struct {
mut:
	value int
}

fn sum(mut s Sum) Sum {
	match mut s {
		Struct {
			return s
		}
		else {
			return s
		}
	}
}

fn test_match_sumtype_var_return_sumtype() {
	mut s := Sum(Struct{
		value: 42
	})
	s = sum(mut s)
	dump(s)
	ret := '${s}'
	assert ret.contains('Sum(Struct{')
	assert ret.contains('value: 42')
}
