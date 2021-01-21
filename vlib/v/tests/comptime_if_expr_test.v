const (
	version              = 123
	disable_opt_features = true
)

// NB: the `unknown_fn()` calls are here on purpose, to make sure that anything
// that doesn't match a compile-time condition is not even parsed.
fn test_ct_expressions() {
	foo := version
	bar := foo
	$if bar == 123 {
		assert true
	} $else {
		unknown_fn()
	}

	$if bar != 123 {
		unknown_fn()
	} $else $if bar != 124 {
		assert true
	} $else {
		unknown_fn()
	}

	$if !disable_opt_features {
		unknown_fn()
	} $else {
		assert true
	}
}

fn generic_t_is<T>() T {
	$if T is string {
		return 'It\'s a string!'
	} $else {
		return T{}
	}
	return T{}
}

struct GenericTIsTest {}

fn test_generic_t_is() {
	assert generic_t_is<string>() == 'It\'s a string!'
	assert generic_t_is<GenericTIsTest>() == GenericTIsTest{}
}

fn generic_t_is2<T>() ?T {
	$if T is string {
		return 'It\'s a string!'
	} $else {
		return T{}
	}
 }

fn test_generic_t_is2() {
	res := generic_t_is2<string>() or {
		assert false
		''
	}
	res2 := generic_t_is2<GenericTIsTest>() or {
		assert false
		GenericTIsTest{}
	}
	assert res == 'It\'s a string!'
	assert res2 == GenericTIsTest{}
}

fn generic_t_is3<T>(raw_data string) ?T {
	$if T is string {
		return ''
	}
	return T{}
}

fn test_generic_t_is3() {
	res := generic_t_is3<GenericTIsTest>('') or {
		assert false
		GenericTIsTest{}
	}
	assert res == GenericTIsTest{}
}

fn test_generic_t_is_with_else() {
	res := generic_t_is_with_else<GenericTIsTest>('') or {
		assert false
		GenericTIsTest{}
	}
	assert res == GenericTIsTest{}
	str := generic_t_is_with_else<string>('test') or {
		assert false
		''
	}
	assert str == 'test'
}

fn generic_t_is_with_else<T>(raw_data string) ?T {
	$if T is string {
		return raw_data
	} $else {
		return T{}
	}
}