type SumType = int | string | f64

struct Foo[T] {
	field ?SumType
}

fn t[T](val Foo[T]) {
	if val.field != none {
		if val.field is string {
			dump(val.field)
			assert val.field == 'foo'
		} else if val.field is int {
			dump(val.field)
			assert val.field == 1
		} else if val.field is f64 {
			dump(val.field)
			assert val.field == 1.23
		}
	} else {
		dump(val.field)
		assert val.field == none
	}
}

fn test_main() {
	t(Foo[int]{})
	t(Foo[string]{})
	t(Foo[f64]{})

	t(Foo[int]{ field: 1 })
	t(Foo[string]{ field: 'foo' })
	t(Foo[f64]{ field: 1.23 })
}
