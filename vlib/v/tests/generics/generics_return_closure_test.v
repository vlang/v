fn vectorize[T](op fn (T) T) fn ([]T) []T {
	return fn [op] [T](values []T) []T {
		mut result := []T{len: values.len}
		for i in 0 .. values.len {
			result[i] = op(values[i])
		}
		return result
	}
}

fn add_one1(x f64) f64 {
	return x + 1
}

fn add_one2(x int) int {
	return x + 1
}

fn test_generic_return_generic_closure() {
	vadd1 := vectorize[f64](add_one1)
	v1 := [1.0, 2, 3, 4]
	println(vadd1(v1))
	assert vadd1(v1) == [2.0, 3, 4, 5]

	vadd2 := vectorize[int](add_one2)
	v2 := [1, 2, 3, 4]
	println(vadd2(v2))
	assert vadd2(v2) == [2, 3, 4, 5]
}

fn result_closure_u64_source() !u64 {
	return u64(0)
}

fn result_closure_enabled() bool {
	return true
}

fn result_closure_disabled() bool {
	return false
}

fn result_closure_choice() int {
	return 0
}

struct ResultClosureField[T] {
	value   u64
	payload T
}

fn make_result_closure_with_local_if_propagation[T]() fn () !T {
	return fn [T]() !T {
		x := if result_closure_enabled() {
			u64(0)
		} else {
			result_closure_u64_source()!
		}
		assert x == u64(0)
		return T(0)
	}
}

fn make_result_closure_with_local_if_or_block[T]() fn () !T {
	return fn [T]() !T {
		x := if result_closure_disabled() {
			u64(1)
		} else {
			result_closure_u64_source() or { u64(1) }
		}
		assert x == u64(0)
		return T(0)
	}
}

fn make_result_closure_with_local_match_propagation[T]() fn () !T {
	return fn [T]() !T {
		x := match result_closure_choice() {
			0 { u64(0) }
			else { result_closure_u64_source()! }
		}

		assert x == u64(0)
		return T(0)
	}
}

fn make_result_closure_returning_if[T]() fn () !T {
	return fn [T]() !T {
		return if result_closure_enabled() {
			T(0)
		} else {
			T(1)
		}
	}
}

fn make_result_closure_returning_match[T]() fn () !T {
	return fn [T]() !T {
		return match result_closure_choice() {
			0 { T(0) }
			else { T(1) }
		}
	}
}

fn make_result_struct_returning_if_field[T](payload T) !ResultClosureField[T] {
	return ResultClosureField[T]{
		value:   if result_closure_enabled() {
			u64(0)
		} else {
			result_closure_u64_source()!
		}
		payload: payload
	}
}

fn make_result_struct_returning_if_field_or_block[T](payload T) !ResultClosureField[T] {
	return ResultClosureField[T]{
		value:   if result_closure_disabled() {
			u64(1)
		} else {
			result_closure_u64_source() or { u64(1) }
		}
		payload: payload
	}
}

fn test_generic_result_closure_local_if_with_propagation() {
	f_int := make_result_closure_with_local_if_propagation[int]()
	assert f_int()! == 0

	f_u64 := make_result_closure_with_local_if_propagation[u64]()
	assert f_u64()! == u64(0)
}

fn test_generic_result_closure_local_if_with_or_block() {
	f := make_result_closure_with_local_if_or_block[int]()
	assert f()! == 0
}

fn test_generic_result_closure_local_match_with_propagation() {
	f := make_result_closure_with_local_match_propagation[int]()
	assert f()! == 0
}

fn test_generic_result_closure_return_if_and_match() {
	if_int := make_result_closure_returning_if[int]()
	assert if_int()! == 0
	if_u64 := make_result_closure_returning_if[u64]()
	assert if_u64()! == u64(0)

	match_int := make_result_closure_returning_match[int]()
	assert match_int()! == 0
	match_u64 := make_result_closure_returning_match[u64]()
	assert match_u64()! == u64(0)
}

fn test_generic_result_struct_return_if_field() {
	field_int := make_result_struct_returning_if_field[int](1)!
	assert field_int.value == u64(0)
	assert field_int.payload == 1

	field_u64 := make_result_struct_returning_if_field_or_block[u64](u64(2))!
	assert field_u64.value == u64(0)
	assert field_u64.payload == u64(2)
}
