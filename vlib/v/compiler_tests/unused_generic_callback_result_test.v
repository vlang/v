module main

// An unused generic callback may retain a Result return type without
// materializing its payload struct. The C function pointer needs only an
// opaque Result declaration, not an invalid by-value Result definition.
struct UnusedCallbackResultPayload[T] {
	value T
}

type UnusedCallbackResultFn[T] = fn (i int) !UnusedCallbackResultPayload[T]

struct HoldsUnusedCallbackResult[T] {
	callback UnusedCallbackResultFn[T] = unsafe { nil }
}

fn test_unused_generic_callback_result_does_not_require_payload_definition() {
	_ = HoldsUnusedCallbackResult[f64]{}
}

// Materialized generic Results still require their complete C definition.
struct UsedCallbackResultPayload[T] {
	value T
}

fn make_used_callback_result[T](value T) !UsedCallbackResultPayload[T] {
	return UsedCallbackResultPayload[T]{
		value: value
	}
}

fn test_materialized_generic_result_retains_its_payload() ! {
	result := make_used_callback_result[int](7)!
	assert result.value == 7
}
