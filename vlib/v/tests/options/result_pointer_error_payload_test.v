struct PointerPayloadError {
	value int
}

fn (e PointerPayloadError) msg() string {
	return 'pointer payload'
}

fn (e PointerPayloadError) code() int {
	return e.value
}

type ResultVoidPointer = voidptr

type ErrorPayloadPointer = &PointerPayloadError

fn pointer_error_voidptr_payload() !voidptr {
	return &PointerPayloadError{ value: 41 }
}

fn pointer_error_alias_payload() !ResultVoidPointer {
	return &PointerPayloadError{ value: 42 }
}

fn aliased_pointer_error_payload() !voidptr {
	value := ErrorPayloadPointer(&PointerPayloadError{ value: 43 })
	return value
}

fn pointer_error_typed_payload() !&PointerPayloadError {
	return &PointerPayloadError{ value: 44 }
}

fn value_error_voidptr_failure() !voidptr {
	return PointerPayloadError{}
}

fn pointer_error_unrelated_failure() !&int {
	return &PointerPayloadError{}
}

fn explicit_error_voidptr_failure() !voidptr {
	return error('explicit failure')
}

fn test_result_pointer_compatible_error_is_a_successful_payload() {
	raw := pointer_error_voidptr_payload() or { panic(err) }
	assert unsafe { (&PointerPayloadError(raw)).value } == 41
	alias := pointer_error_alias_payload() or { panic(err) }
	assert unsafe { (&PointerPayloadError(alias)).value } == 42
	pointer_alias := aliased_pointer_error_payload() or { panic(err) }
	assert unsafe { (&PointerPayloadError(pointer_alias)).value } == 43
	typed := pointer_error_typed_payload() or { panic(err) }
	assert typed.value == 44
}

fn test_result_pointer_incompatible_errors_remain_failures() {
	mut failures := 0
	if _ := value_error_voidptr_failure() {
		assert false, 'a value error cannot be a pointer payload'
	} else {
		assert err.msg() == 'pointer payload'
		failures++
	}
	if _ := pointer_error_unrelated_failure() {
		assert false, 'an unrelated pointer cannot be the success payload'
	} else {
		assert err.msg() == 'pointer payload'
		failures++
	}
	if _ := explicit_error_voidptr_failure() {
		assert false, 'explicit errors must remain failures'
	} else {
		assert err.msg() == 'explicit failure'
		failures++
	}
	assert failures == 3
}
