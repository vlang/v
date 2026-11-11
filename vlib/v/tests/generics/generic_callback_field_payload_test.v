struct CallbackInput[T] {
	value T
}

struct CallbackResult[T] {
	value T
}

type BatchCallback[T] = fn (input &CallbackInput[T]) !CallbackResult[T]

type OptionalBatchCallback[T] = fn (input CallbackInput[T]) ?CallbackResult[T]

struct CallbackHolder[T] {
	value    T
	batch    BatchCallback[T]         = unsafe { nil }
	optional OptionalBatchCallback[T] = unsafe { nil }
}

fn test_generic_callback_field_materializes_its_payload_types() {
	// Neither callback is called: only the field signatures require the concrete
	// CallbackInput[f64] and CallbackResult[f64] declarations.
	holder := CallbackHolder[f64]{ value: 3.5 }
	assert holder.value == 3.5
	assert holder.batch == unsafe { nil }
	assert holder.optional == unsafe { nil }
}
