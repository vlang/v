pub struct Maybe[T] {
	present bool
	val     T
}

pub fn some[T](t T) Maybe[T] {
	return Maybe[T]{true, t}
}

pub fn (me Maybe[T]) map[T, U](f fn (T) U) Maybe[U] {
	if me.present {
		return Maybe[U]{true, f(me.val)}
	}
	return Maybe[U]{}
}

pub fn (o Maybe[T]) get() ?T {
	if o.present {
		return o.val
	} else {
		return none
	}
}

fn test_generics_struct_init_with_inconsistent_generic_types() {
	m := some[int](12)
	ret := m.map[int, string](fn (i int) string {
		return i.str()
	})
	println(ret)
	assert ret.val == '12'
}
