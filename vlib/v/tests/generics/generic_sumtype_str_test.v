struct None {}

pub type Maybe[T] = None | T

struct MaybeContainer[T] {
	value Maybe[T]
}

struct MaybeOptionalContainer[T] {
	value ?Maybe[T]
}

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

fn generic_match_has_value[T](m Maybe[T]) bool {
	return match m {
		T {
			true
		}
		else {
			false
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
	assert ref_sum.str() == 'Some(${ptr_str(ptr)})'
	assert '${ref_sum}' == 'Some(${ptr_str(ptr)})'

	sum := some(123)
	sum_ref := sum.as_ref()
	assert typeof(sum_ref).name == 'Maybe[&int]'
	sum_ref_str := sum_ref.str()
	assert '${sum_ref}' == sum_ref_str
	if sum_ref is &int {
		assert sum_ref_str == 'Some(${ptr_str(sum_ref)})'
	} else {
		assert false
	}
}

fn test_generic_sumtype_match_resolves_variant_type() {
	assert generic_match_has_value(some(123))
	assert !generic_match_has_value(noth[int]())
}

fn test_auto_str_struct_field_with_generic_sumtype_str() {
	c := MaybeContainer[int]{
		value: some(456)
	}
	assert '${c}'.contains('value: Some(456)')
}

fn test_auto_str_optional_field_with_generic_sumtype_str() {
	c := MaybeOptionalContainer[int]{
		value: some(789)
	}
	assert '${c}'.contains('Some(789)')
}
