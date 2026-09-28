fn type_kind[T](_ T) string {
	$if T is $enum {
		return 'enum'
	} $else $if T is $option {
		return 'option'
	} $else $if T is $pointer {
		return 'pointer'
	} $else {
		return 'other'
	}
}

struct MultiPointers {
	a ?&&int
	b ?&&&string
	c &&int = unsafe { nil }
}

// The `&&` of a substituted `?&&int` is part of the type, not a logical AND.
fn test_comptime_conditions_on_option_multi_pointer_types() {
	value := 1
	ptr := &value
	ptr_ptr := &ptr
	holder := MultiPointers{
		a: ptr_ptr
		c: ptr_ptr
	}
	assert type_kind(holder.a) == 'option'
	assert type_kind(holder.b) == 'option'
	assert type_kind(holder.c) == 'pointer'
}
