pub struct Time[T] {
pub mut:
	start T
	end   T
}

pub struct Transform[T] {
pub mut:
	time   Time[T]
	before []T
	after  []T
}

pub fn (t Transform[T]) clone() Transform[T] {
	return Transform[T]{
		...t
	}
}

fn test_generic_struct_init_with_update_expr() {
	a := Transform[f64]{
		before: [0.0, 0.0]
		after: [320.0, 240.0]
	}

	b := a.clone()

	println(b)
	assert b.before == a.before
	assert b.after == a.after
}
