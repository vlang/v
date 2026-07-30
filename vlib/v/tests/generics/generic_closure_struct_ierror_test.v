struct GenericClosureOutcome[T] {
	value T
	err   IError = none
}

fn generic_closure_retry[T](op fn () GenericClosureOutcome[T]) GenericClosureOutcome[T] {
	result := op()
	if result.err is none {
		return result
	}
	return GenericClosureOutcome[T]{
		value: result.value
		err:   error('fail: ${result.err.msg()}')
	}
}

fn test_generic_closure_returning_struct_with_ierror_field() {
	mut count := 0
	operation := fn [mut count] () GenericClosureOutcome[int] {
		count++
		if count == 3 {
			return GenericClosureOutcome[int]{
				value: 42
			}
		}
		return GenericClosureOutcome[int]{
			err: error('boom ${count}')
		}
	}
	result := generic_closure_retry(operation)
	assert result.value == 0
	assert result.err.msg() == 'fail: boom 1'
}
