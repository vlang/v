struct Foo[T] {
	a T
}

fn r[T]() Foo[T] {
	return Foo[T]{}
}

fn t[T](v T) !Foo[T] {
	return match typeof(v).name {
		'string' {
			r[T]()
		}
		else {
			r[T]()
		}
	}
}

fn test_main() {
	t(1)!
	t('')!
	assert true
}

type GenericMatchSumtype[T] = GenericMatchY[T] | GenericMatchZ[T]

fn (mut x GenericMatchSumtype[T]) do_it[T]() string {
	match mut x {
		GenericMatchY[T] { return 'doing GenericMatchY' }
		GenericMatchZ[T] { return 'doing GenericMatchZ' }
	}

	return ''
}

struct GenericMatchY[T] {
	name  string
	value T
}

struct GenericMatchZ[T] {
	name  string
	value T
}

fn test_match_generic_sumtype_variant_in_generic_method() {
	y := GenericMatchY[int]{
		name:  'Y'
		value: 1
	}
	z := GenericMatchZ[bool]{
		name:  'Z'
		value: true
	}

	mut x1 := GenericMatchSumtype[int](y)
	assert x1.do_it() == 'doing GenericMatchY'

	mut x2 := GenericMatchSumtype[bool](z)
	assert x2.do_it() == 'doing GenericMatchZ'
}

type MatchedGenericMethodBox[T] = MatchedGenericMethodY[T] | MatchedGenericMethodZ[T]

struct MatchedGenericMethodY[T] {
	value T
}

struct MatchedGenericMethodZ[T] {
	value T
}

fn (x MatchedGenericMethodBox[T]) explicit_variant_name[T]() string {
	match x {
		MatchedGenericMethodY[T] { return x.explicit_variant_name[T]() }
		MatchedGenericMethodZ[T] { return x.explicit_variant_name[T]() }
	}

	return ''
}

fn (y MatchedGenericMethodY[T]) explicit_variant_name[T]() string {
	return 'Y'
}

fn (z MatchedGenericMethodZ[T]) explicit_variant_name[T]() string {
	return 'Z'
}

fn (x MatchedGenericMethodBox[T]) inferred_variant_name[T]() string {
	match x {
		MatchedGenericMethodY[T] { return x.inferred_variant_name() }
		MatchedGenericMethodZ[T] { return x.inferred_variant_name() }
	}

	return ''
}

fn (x MatchedGenericMethodBox[T]) match_expr_variant_name[T]() string {
	return match x {
		MatchedGenericMethodY[T] { x.explicit_variant_name[T]() }
		MatchedGenericMethodZ[T] { x.explicit_variant_name[T]() }
	}
}

fn (y MatchedGenericMethodY[T]) inferred_variant_name[T]() string {
	return 'Y'
}

fn (z MatchedGenericMethodZ[T]) inferred_variant_name[T]() string {
	return 'Z'
}

fn test_generic_sumtype_match_calls_explicit_generic_method_on_variant() {
	y := MatchedGenericMethodBox[int](MatchedGenericMethodY[int]{
		value: 1
	})
	assert y.explicit_variant_name() == 'Y'

	z := MatchedGenericMethodBox[bool](MatchedGenericMethodZ[bool]{
		value: true
	})
	assert z.explicit_variant_name() == 'Z'
}

fn test_generic_sumtype_match_calls_explicit_generic_method_on_variant_reversed() {
	z := MatchedGenericMethodBox[bool](MatchedGenericMethodZ[bool]{
		value: true
	})
	assert z.explicit_variant_name() == 'Z'

	y := MatchedGenericMethodBox[int](MatchedGenericMethodY[int]{
		value: 1
	})
	assert y.explicit_variant_name() == 'Y'
}

fn test_generic_sumtype_match_calls_explicit_generic_method_single_instantiation() {
	y := MatchedGenericMethodBox[string](MatchedGenericMethodY[string]{
		value: 'one'
	})
	assert y.explicit_variant_name() == 'Y'

	z := MatchedGenericMethodBox[string](MatchedGenericMethodZ[string]{
		value: 'two'
	})
	assert z.explicit_variant_name() == 'Z'
}

fn test_generic_sumtype_match_calls_inferred_generic_method_on_variant() {
	z := MatchedGenericMethodBox[int](MatchedGenericMethodZ[int]{
		value: 1
	})
	assert z.inferred_variant_name() == 'Z'

	y := MatchedGenericMethodBox[bool](MatchedGenericMethodY[bool]{
		value: true
	})
	assert y.inferred_variant_name() == 'Y'
}

fn test_generic_sumtype_match_expr_calls_generic_method_on_last_variant() {
	y := MatchedGenericMethodBox[int](MatchedGenericMethodY[int]{
		value: 1
	})
	assert y.match_expr_variant_name() == 'Y'

	z := MatchedGenericMethodBox[bool](MatchedGenericMethodZ[bool]{
		value: true
	})
	assert z.match_expr_variant_name() == 'Z'
}
