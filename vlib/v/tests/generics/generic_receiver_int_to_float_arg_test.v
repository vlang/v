// An integer passed to a float parameter of a call in a generic body converts
// as it does outside one: `app.record(30)` for a `record(value f64)`.
struct Recorder {
	scale fn (value f64) f64 = unsafe { nil }
mut:
	calls int
}

fn (mut r Recorder) record(value f64) f64 {
	r.calls++
	return value
}

fn (r Recorder) half(value f32) f32 {
	return value / 2
}

fn record_literal[A](mut app A) f64 {
	return app.record(30)
}

fn record_in_closure[A](mut app A) f64 {
	read := fn [mut app] [A]() f64 {
		return app.record(30)
	}
	return read()
}

fn record_int_value[A](mut app A) f64 {
	count := 7
	return app.record(count)
}

fn half_negative[A](app A) f32 {
	return app.half(-4)
}

fn scale_field[A](app A) f64 {
	return app.scale(5)
}

fn test_integer_arguments_convert_to_float_parameters_in_generic_bodies() {
	mut r := Recorder{
		scale: fn (value f64) f64 {
			return value * 10
		}
	}
	assert record_literal(mut r) == 30.0
	assert record_in_closure(mut r) == 30.0
	assert record_int_value(mut r) == 7.0
	assert r.calls == 3
	assert half_negative(r) == -2.0
	assert scale_field(r) == 50.0
}
