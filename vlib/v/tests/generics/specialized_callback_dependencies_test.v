import math

struct Tensor[T] {
	data []T
}

fn (t &Tensor[T]) apply[T](callback fn (T) T) &Tensor[T] {
	return &Tensor[T]{ data: t.data.map(callback(it)) }
}

fn sigmoid[T](t &Tensor[T]) &Tensor[T] {
	return t.apply(fn [T](value T) T { return T(math.exp(f64(value))) })
}

fn unused_sigmoid(t &Tensor[f32]) &Tensor[f32] { return sigmoid[f32](t) }

fn test_unused_specialized_callback() {
	t := &Tensor[f64]{ data: [f64(0)] }
	assert t.apply(fn (value f64) f64 { return value + 1 }).data == [f64(1)]
}

fn test_used_specialized_callback_dependencies() {
	t := &Tensor[f64]{ data: [f64(0)] }
	assert t.apply(fn (value f64) f64 { return math.cos(value) }).data == [f64(1)]
}
