import math
import time { now }

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

fn fixed_array_comparison_after_callback(a [1]int) bool {
	callback := fn (value int) int { return value + 1 }
	assert callback(0) == 1
	return a == [1]!
}

fn test_fixed_array_parameter_survives_callback_dependency_scan() {
	assert fixed_array_comparison_after_callback([1]!)
}

fn test_lifted_callback_initializer_keeps_global_callee() {
	t := &Tensor[f64]{ data: [f64(1)] }
	result := t.apply(fn (value f64) f64 {
		now := now()
		_ = now
		return value + 1
	})
	assert result.data == [f64(2)]
}

fn callback_scope_global(value f64) f64 {
	return value + 1
}

fn test_lifted_callback_keeps_global_after_nested_local_scope() {
	t := &Tensor[f64]{ data: [f64(1)] }
	result := t.apply(fn (value f64) f64 {
		if value > 0 {
			callback_scope_global := 1
			_ = callback_scope_global
		}
		return callback_scope_global(value)
	})
	assert result.data == [f64(2)]
}

fn test_lifted_callback_keeps_global_after_c_style_loop_scope() {
	t := &Tensor[f64]{ data: [f64(1)] }
	result := t.apply(fn (value f64) f64 {
		for callback_scope_global := 0; callback_scope_global < 1; callback_scope_global++ {
		}
		return callback_scope_global(value)
	})
	assert result.data == [f64(2)]
}

fn test_lifted_callback_keeps_global_after_for_in_scope() {
	t := &Tensor[f64]{ data: [f64(1)] }
	result := t.apply(fn (value f64) f64 {
		for callback_scope_global, item in {
			'x': 1
		} {
			_ = callback_scope_global
			_ = item
		}
		return callback_scope_global(value)
	})
	assert result.data == [f64(2)]
}

fn test_lifted_callback_keeps_global_after_select_branch_scope() {
	t := &Tensor[f64]{ data: [f64(1)] }
	result := t.apply(fn (value f64) f64 {
		ch := chan f64{cap: 1}
		ch <- value
		select {
			got := <-ch {
				callback_scope_global := got
				_ = callback_scope_global
			}
			0 * time.millisecond {
			}
		}
		return callback_scope_global(value)
	})
	assert result.data == [f64(2)]
}
