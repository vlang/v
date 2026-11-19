// Regression: a captured void callback may be used where the callback slot
// returns !void. The generated thunk must preserve both the captured context
// and the mutable argument's ABI (Result<void> can use a hidden return pointer).
type CapturedResultCallback = fn (mut target CapturedResultTarget) !

struct CapturedResultTarget {
mut:
	value int
}

fn invoke_captured_result_callback(callback CapturedResultCallback, mut target CapturedResultTarget) ! {
	callback(mut target)!
}

fn test_captured_void_callback_promotes_result_without_shifting_mut_argument() ! {
	mut target := &CapturedResultTarget{}
	increment := 17
	invoke_captured_result_callback(fn [increment] (mut item CapturedResultTarget) {
		item.value += increment
	}, mut target)!
	assert target.value == 17
}

fn test_captured_fallible_callback_preserves_existing_result_abi() ! {
	mut target := &CapturedResultTarget{}
	increment := 23
	invoke_captured_result_callback(fn [increment] (mut item CapturedResultTarget) ! {
		item.value += increment
	}, mut target)!
	assert target.value == 23
}
