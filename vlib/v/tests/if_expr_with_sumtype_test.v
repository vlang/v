struct A_str {}

struct B_str {}

type Token = A_str | B_str

fn next(mut v []Token) Token {
	return if v.len > 0 { v.pop() } else { A_str{} }
}

fn test_if_expr_with_sumtype() {
	mut arr := []Token{}
	ret1 := next(mut arr)
	println(ret1)
	assert '$ret1' == 'Token(A_str{})'

	arr << A_str{}
	arr << B_str{}

	ret2 := next(mut arr)
	println(ret2)
	assert '$ret2' == 'Token(B_str{})'
}
