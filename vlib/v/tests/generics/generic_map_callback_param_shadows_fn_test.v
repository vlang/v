// The program has a function `callback` that returns `?bool`. In the generic
// functions, `callback` is a parameter, and `.map(callback(it))` maps to its result.
fn callback(x int) ?bool {
	return x > 0
}

struct Tensor[T] {
	data []T
}

fn (t &Tensor[T]) apply[T](callback fn (T) T) []T {
	return t.data.map(callback(it))
}

fn apply_plain[T](data []T, callback fn (T) T) []T {
	return data.map(callback(it))
}

fn test_map_of_a_callback_parameter_named_like_a_function() {
	assert callback(1) or { false }
	t := &Tensor[f64]{
		data: [f64(1)]
	}
	assert t.apply(fn (v f64) f64 {
		return v + 1
	}) == [f64(2)]
	assert apply_plain([1, 2], fn (v int) int {
		return v * 2
	}) == [2, 4]
}
