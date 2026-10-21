struct Cov {
mut:
	count int
}

struct Rows[T] {
	items []T
}

fn (rows Rows[T]) count() int {
	return rows.items.len
}

fn rows(mut cov Cov) Rows[string] {
	cov.count++
	return Rows[string]{ items: ['ok'] }
}

fn parse() ?&Cov {
	return &Cov{}
}

fn test_mut_if_guard_argument_in_generic_method_receiver() {
	if mut cov := parse() {
		assert rows(mut cov).count() == 1
		assert cov.count == 1
	} else {
		assert false
	}
}
