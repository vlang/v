type Value = voidptr

pub fn create[T](val T) Value {
	$if T is bool {
		println('bool ${val}')
		return create_value_from_bool(val)
	} $else {
		println('not bool ${val}')
	}

	return Value(unsafe { nil })
}

fn create_value_from_bool(val bool) Value {
	return Value(voidptr(val))
}

fn test_main() {
	mut val1 := create(true)
	assert val1 == Value(voidptr(1))

	val1 = create(1.2)
	assert val1 == Value(unsafe { nil })

	val1 = create(1)
	assert val1 == Value(unsafe { nil })

	val1 = create('')
	assert val1 == Value(unsafe { nil })
}
