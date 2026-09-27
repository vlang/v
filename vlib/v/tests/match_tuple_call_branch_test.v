import os

enum Kind {
	one
	two
}

fn bounds() (int, int) { return 3, 4 }

fn pick(k Kind) (int, int) {
	start, end := match k {
		.one {
			n := 1
			n, 2
		}
		.two { bounds() }
	}
	return start, end
}

fn test_match_tuple_call() {
	a, b := pick(.one)
	c, d := pick(.two)
	assert [a, b, c, d] == [1, 2, 3, 4]
}

fn pick_call_first(k Kind) (int, int) {
	return match k {
		.one { bounds() }
		.two { 5, 6 }
	}
}

fn labelled_bounds() (int, string) {
	return 7, 'seven'
}

fn pick_label(k Kind) (int, string) {
	return match k {
		.one { 8, 'eight' }
		.two { labelled_bounds() }
	}
}

fn test_match_tuple_call_order_and_element_types() {
	a, b := pick_call_first(.one)
	c, d := pick_call_first(.two)
	assert [a, b, c, d] == [3, 4, 5, 6]
	n, text := pick_label(.one)
	other_n, other_text := pick_label(.two)
	assert n == 8 && text == 'eight'
	assert other_n == 7 && other_text == 'seven'
}

fn test_match_tuple_comma_tail_promotes_one_component() {
	for flag in [true, false] {
		a, b := match flag {
			true { 1, 0 }
			else { 1.5, 0 }
		}
		if flag {
			assert a == 1.0
		} else {
			assert a == 1.5
		}
		assert b == 0
	}
}

fn wrapped_pair() (int, ?string) {
	return 1, 'ok'
}

fn wrapped_result_pair() (int, !string) {
	return 1, 'ok'
}

fn ierror_value() IError {
	return error('payload')
}

fn wrapped_ierror_pair() (int, !IError) {
	return 1, ierror_value()
}

fn test_match_tuple_explicit_error_is_result_failure() {
	for flag in [true, false] {
		value, item := match flag {
			true { wrapped_ierror_pair() }
			else { 2, error('boom') }
		}
		assert value == if flag { 1 } else { 2 }
		if got := item {
			assert flag
			assert got.msg() == 'payload'
		} else {
			assert !flag
		}
	}
}

fn test_match_tuple_parenthesized_error_is_result_failure() {
	for flag in [true, false] {
		_, item := match flag {
			true { wrapped_ierror_pair() }
			else { 2, (error('boom')) }
		}
		if got := item {
			assert flag
			assert got.msg() == 'payload'
		} else {
			assert !flag
		}
	}
}

fn test_match_tuple_error_promotes_to_optional_slot() {
	for flag in [true, false] {
		value, text := match flag {
			true { wrapped_pair() }
			else { 2, error('x') }
		}
		if flag {
			assert value == 1
			assert (text or { '' }) == 'ok'
		} else {
			assert value == 2
			assert (text or { 'failed' }) == 'failed'
		}
	}
}

fn test_match_tuple_none_promotes_to_optional_slot_in_both_orders() {
	for flag in [true, false] {
		first_value, first_text := match flag {
			true { wrapped_pair() }
			else { 2, none }
		}
		second_value, second_text := match flag {
			true { 2, none }
			else { wrapped_pair() }
		}
		assert first_value == if flag { 1 } else { 2 }
		assert second_value == if flag { 2 } else { 1 }
		assert (first_text or { 'missing' }) == if flag { 'ok' } else { 'missing' }
		assert (second_text or { 'missing' }) == if flag { 'missing' } else { 'ok' }
	}
}

fn test_match_tuple_rejects_all_none_non_final_slot() {
	path := os.join_path(os.vtmp_dir(), 'v3_all_none_tuple_slot_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn main() { a, b := match true { true { none, 1 } else { none, 2 } }; _ = a; _ = b }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot assign a `none` value to a variable'), result.output
}

fn test_match_tuple_error_promotes_to_result_slot() {
	for flag in [true, false] {
		value, text := match flag {
			true { wrapped_result_pair() }
			else { 2, error('x') }
		}
		if flag {
			assert value == 1
			assert (text or { '' }) == 'ok'
		} else {
			assert value == 2
			assert (text or { 'failed' }) == 'failed'
		}
	}
}

fn test_if_tuple_error_promotes_to_wrapped_slot() {
	for flag in [true, false] {
		value, text := if flag { wrapped_pair() } else { 2, error('x') }
		assert value == if flag { 1 } else { 2 }
		assert (text or { 'failed' }) == if flag { 'ok' } else { 'failed' }

		result_value, result_text := if flag { wrapped_result_pair() } else { 3, error('x') }
		assert result_value == if flag { 1 } else { 3 }
		assert (result_text or { 'failed' }) == if flag { 'ok' } else { 'failed' }
	}
}
