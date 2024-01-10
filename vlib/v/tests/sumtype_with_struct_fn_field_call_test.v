type Expr = Fun | int

struct Fun {
	f fn (int) int
}

fn test_sumtype_with_struct_fn_field_call() {
	f := Expr(0)
	if f is Fun {
		println((f as Fun).f(0))
	}
	assert true
}
