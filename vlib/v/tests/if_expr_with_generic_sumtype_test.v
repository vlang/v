type Opt<T> = None<T> | Some<T>

struct None<T> {}

struct Some<T> {
mut:
	value T
}

fn operation(r int) Opt<int> {
	return if r > 0 { Some<int>{r} } else { None<int>{} }
}

fn test_if_expr_with_generic_sumtype() {
	op := operation(1)
	assert Opt<int>(Some<int>{1}) == op
}
