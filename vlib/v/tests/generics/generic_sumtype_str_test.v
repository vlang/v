struct None {}

pub type Maybe[T] = None | T

pub fn (m Maybe[T]) str[T]() string {
	return if m is T {
		x := m as T
		'Some(${x})'
	} else {
		'Noth'
	}
}

pub fn some[T](v T) Maybe[T] {
	return Maybe[T](v)
}

pub fn noth[T]() Maybe[T] {
	return Maybe[T](None{})
}

pub fn (m &Maybe[T]) as_ref[T]() Maybe[&T] {
	return match m {
		None {
			noth[&T]()
		}
		T {
			mut ref := voidptr(unsafe { &m })
			some[&T](ref)
		}
	}
}

fn test_generic_sumtype_str() {
	a := some(123)
	b := some('abc')

	println(a.str())
	println(a)
	println('${a}')
	assert '${a}' == 'Some(123)'

	println(b.str())
	println(b)
	println('${b}')
	assert '${b}' == 'Some(abc)'
}

fn test_generic_sumtype_str_with_ref_variant() {
	value := 123
	ptr := &value
	ref_sum := some[&int](ptr)
	assert typeof(ref_sum).name == 'Maybe[&int]'
	assert ref_sum.str() == 'Some(123)'
	assert '${ref_sum}' == 'Some(123)'

	sum := some(123)
	sum_ref := sum.as_ref()
	assert typeof(sum_ref).name == 'Maybe[&int]'
	assert sum_ref.str() == 'Some(123)'
	assert '${sum_ref}' == 'Some(123)'
}
