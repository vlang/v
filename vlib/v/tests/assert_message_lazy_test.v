struct LazyAssertCounter {
mut:
	calls int
}

struct LazyAssertPoint {
	value   int
	counter &LazyAssertCounter
}

fn (point LazyAssertPoint) str() string {
	// The counter belongs to the test and remains alive for every formatting call.
	unsafe { point.counter.calls++ }
	return 'Point(${point.value})'
}

fn lazy_assert_message(mut counter LazyAssertCounter) string {
	counter.calls++
	return 'message'
}

fn test_passing_assert_does_not_format_array_message() {
	mut counter := LazyAssertCounter{}
	points := [LazyAssertPoint{1, &counter}, LazyAssertPoint{2, &counter}]
	for i in 0 .. 3 {
		assert points.len == 2, 'pass ${i}: ${points}'
	}
	assert counter.calls == 0
}

fn test_passing_assert_does_not_call_interpolation_operands() {
	mut counter := LazyAssertCounter{}
	assert true, 'message: ${lazy_assert_message(mut counter)}'
	assert counter.calls == 0
	assert true, lazy_assert_message(mut counter)
	assert counter.calls == 0
}

fn test_passing_assert_does_not_evaluate_message_match() {
	mut counter := LazyAssertCounter{}
	assert true, match counter.calls {
		0 { 'message: ${lazy_assert_message(mut counter)}' }
		else { lazy_assert_message(mut counter) }
	}
	assert counter.calls == 0
}
