// An array literal that is built through a temporary pushes its elements one by
// one, after declaring the temporary. A block element such as `unsafe { ... }`
// transforms its own statements eagerly, and used to drain that declaration into
// the block, so the C the backend emitted declared `__arr_lit_0` inside an
// expression and the `array_push` calls after it did not compile:
//
//	error: use of undeclared identifier '__arr_lit_0'
//
// `vlib/v/util/vwatchtty` returns its signal numbers exactly this way, which is
// what broke `v watch`.

fn stop_signals() []int {
	return [unsafe { int(1) }, unsafe { int(2) }, unsafe { int(3) }]
}

fn test_every_unsafe_element_is_kept() {
	assert stop_signals() == [1, 2, 3]
}

fn test_a_single_unsafe_element() {
	assert [unsafe { int(7) }] == [7]
}

fn test_unsafe_elements_mixed_with_plain_ones() {
	assert [1, unsafe { int(2) }, 3] == [1, 2, 3]
	assert [unsafe { int(1) }, 2, unsafe { int(3) }] == [1, 2, 3]
}

struct Recorder {
mut:
	seen []int
}

fn (mut r Recorder) next(x int) int {
	r.seen << x
	return x
}

// The temporary exists so that the elements keep V's left to right evaluation
// order, so the fix has to preserve it rather than just compile.
fn test_unsafe_elements_keep_their_order() {
	mut r := Recorder{}
	values := [unsafe { r.next(1) }, unsafe { r.next(2) }, unsafe { r.next(3) }]
	assert values == [1, 2, 3]
	assert r.seen == [1, 2, 3]
}

fn test_unsafe_elements_in_a_string_array() {
	prefix := 'v'
	assert [unsafe { prefix + '1' }, unsafe { prefix + '2' }] == ['v1', 'v2']
}
