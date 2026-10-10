import rand

// These cover the validation surface of the module level API: every function
// that takes a bound has to refuse a bound that would make its result
// meaningless, and the refusal has to be an error rather than a panic.
// `assert_error` is strict in both directions: a missing error is a failure.

fn assert_error(expected string, f fn () !) {
	f() or {
		assert err.msg() == expected
		return
	}
	assert false, 'expected an error, but the call succeeded'
}

fn test_bytes_rejects_a_negative_count() {
	assert_error('can not read < 0 random bytes', fn () ! {
		rand.bytes(-1)!
	})
}

fn test_bytes_zero_returns_an_empty_buffer() {
	empty := rand.bytes(0) or { panic(err) }
	assert empty.len == 0
}

fn test_intn_rejects_a_non_positive_max() {
	limit := 'max has to be positive.'
	assert_error(limit, fn () ! {
		rand.intn(0)!
	})
	assert_error(limit, fn () ! {
		rand.intn(-3)!
	})
	assert_error(limit, fn () ! {
		rand.i32n(0)!
	})
	assert_error(limit, fn () ! {
		rand.i64n(0)!
	})
}

fn test_u32n_and_u64n_reject_a_zero_max() {
	limit := 'max must be positive integer'
	assert_error(limit, fn () ! {
		rand.u32n(0)!
	})
	assert_error(limit, fn () ! {
		rand.u64n(0)!
	})
}

fn test_integer_range_functions_reject_an_empty_or_inverted_range() {
	limit := 'max must be greater than min'
	assert_error(limit, fn () ! {
		rand.int_in_range(5, 5)!
	})
	assert_error(limit, fn () ! {
		rand.int_in_range(9, 3)!
	})
	assert_error(limit, fn () ! {
		rand.i32_in_range(5, 5)!
	})
	assert_error(limit, fn () ! {
		rand.i64_in_range(5, 5)!
	})
	assert_error(limit, fn () ! {
		rand.u32_in_range(5, 5)!
	})
	assert_error(limit, fn () ! {
		rand.u64_in_range(5, 5)!
	})
}

fn test_negative_range_bounds_are_supported() {
	for _ in 0 .. 64 {
		v := rand.int_in_range(-10, -5) or { panic(err) }
		assert v >= -10 && v < -5
	}
}

fn test_scaled_float_functions_reject_a_negative_max() {
	limit := 'max has to be non-negative.'
	assert_error(limit, fn () ! {
		rand.f32n(-1)!
	})
	assert_error(limit, fn () ! {
		rand.f64n(-1)!
	})
}

fn test_float_range_functions_reject_an_inverted_range() {
	limit := 'max must be greater than or equal to min'
	assert_error(limit, fn () ! {
		rand.f32_in_range(2, 1)!
	})
	assert_error(limit, fn () ! {
		rand.f64_in_range(2, 1)!
	})
}

fn test_bernoulli_rejects_a_probability_outside_0_to_1() {
	assert_error('-0.5 is not a valid probability value.', fn () ! {
		rand.bernoulli(-0.5)!
	})
	assert_error('1.5 is not a valid probability value.', fn () ! {
		rand.bernoulli(1.5)!
	})
}

fn test_binomial_rejects_a_probability_outside_0_to_1() {
	assert_error('-0.5 is not a valid probability value.', fn () ! {
		rand.binomial(4, -0.5)!
	})
	assert_error('1.5 is not a valid probability value.', fn () ! {
		rand.binomial(4, 1.5)!
	})
}

fn test_normal_and_normal_pair_reject_a_non_positive_sigma() {
	limit := 'Standard deviation must be positive'
	assert_error(limit, fn () ! {
		rand.normal(sigma: 0.0)!
	})
	assert_error(limit, fn () ! {
		rand.normal(sigma: -1.0)!
	})
	assert_error(limit, fn () ! {
		rand.normal_pair(sigma: 0.0)!
	})
	assert_error(limit, fn () ! {
		rand.normal_pair(sigma: -1.0)!
	})
}

fn test_choose_rejects_a_sample_larger_than_the_array() {
	assert_error('Cannot choose 4 elements without replacement from a 3-element array.',
		fn () ! {
			rand.choose([1, 2, 3], 4)!
		})
}

fn test_element_rejects_an_empty_array() {
	assert_error('Cannot choose an element from an empty array.', fn () ! {
		rand.element([]int{})!
	})
}

fn test_shuffle_rejects_a_negative_start() {
	mut a := [1, 2, 3]
	rand.shuffle(mut a, start: -1) or {
		assert err.msg() == "argument 'config.start' must be in range [0, a.len)"
		return
	}
	assert false, 'expected an error, but the call succeeded'
}

fn test_shuffle_rejects_a_start_at_the_array_length() {
	mut a := [1, 2, 3]
	rand.shuffle(mut a, start: 3) or {
		assert err.msg() == "argument 'config.start' must be in range [0, a.len)"
		return
	}
	assert false, 'expected an error, but the call succeeded'
}

fn test_shuffle_rejects_a_negative_end() {
	mut a := [1, 2, 3]
	rand.shuffle(mut a, start: 0, end: -1) or {
		assert err.msg() == "argument 'config.end' must be in range [0, a.len]"
		return
	}
	assert false, 'expected an error, but the call succeeded'
}

fn test_shuffle_rejects_an_end_at_or_below_the_start() {
	mut a := [1, 2, 3]
	rand.shuffle(mut a, start: 1, end: 1) or {
		assert err.msg() == "argument 'config.end' must be greater than 'config.start'"
		return
	}
	assert false, 'expected an error, but the call succeeded'
}

fn test_shuffle_clone_propagates_the_validation_error() {
	assert_error("argument 'config.end' must be greater than 'config.start'", fn () ! {
		rand.shuffle_clone([1, 2, 3], start: 2, end: 1)!
	})
}

fn test_sample_accepts_zero_and_a_larger_than_the_array_k() {
	assert rand.sample([1, 2, 3], 0).len == 0
	assert rand.sample([1, 2, 3], 5).len == 5
}

fn test_zero_length_string_helpers_return_an_empty_string() {
	assert rand.string_from_set('abc', 0) == ''
	assert rand.hex(0) == ''
	assert rand.string(0).len == 0
	assert rand.ascii(0).len == 0
}
