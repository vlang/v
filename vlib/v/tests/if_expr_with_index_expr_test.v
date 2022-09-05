import rand

fn test_if_expr_with_index_expr() {
	a := [1, 2, 3]

	b := if true { a[rand.intn(a.len) or { 0 }] } else { 0 }
	println(b)
	assert true
}
