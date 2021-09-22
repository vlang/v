const (
	str = 'abcdefghijklmnopqrstuvwxyz'
	num = 1234567890
)

struct S1 {
	s1 string = str
}

struct S2 {
	s2 int = num
}

type Sum = S1 | S2

fn test_as_cast_with_sumtype_fn_result() {
	a := [Sum(S1{}), Sum(S2{})]
	v1 := a.first() as S1
	assert v1.s1 == str
	assert (a.first() as S1).s1 == str
	v2 := a.last() as S2
	assert v2.s2 == num
	assert (a.last() as S2).s2 == num
}

fn test_is_expr_with_sumtype_fn_result() {
	a := [Sum(S1{}), Sum(S2{})]
	assert a.first() is S1
	assert a.last() is S2
}
