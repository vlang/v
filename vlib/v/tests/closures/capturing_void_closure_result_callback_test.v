// A capturing closure that returns nothing, passed where a `fn (...) !` callback is
// expected, must still receive its arguments and report success to the caller.
@[heap]
struct ResultCallbackTarget {
mut:
	hits int
}

type ResultCallback = fn (mut target ResultCallbackTarget) !

fn run_result_callback(mut target ResultCallbackTarget, callback ResultCallback) ! {
	callback(mut target)!
}

fn test_capturing_void_closure_as_result_callback() {
	step := 7
	mut target := &ResultCallbackTarget{}
	for _ in 0 .. 3 {
		run_result_callback(mut target, fn [step] (mut target ResultCallbackTarget) {
			target.hits += step
		}) or { assert false, 'a void callback reported an error: ${err}' }
	}
	assert target.hits == 21
}
