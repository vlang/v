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

fn test_match_tuple_value_block_error_is_result_failure() {
	for flag in [true, false] {
		_, item := match flag {
			true { wrapped_ierror_pair() }
			else { 2, unsafe { error('boom') } }
		}
		if got := item {
			assert flag
			assert got.msg() == 'payload'
		} else {
			assert !flag
		}
	}
}

interface Any {}

fn wrapped_any_pair() (int, !Any) {
	return 1, Any('ok')
}

fn test_match_tuple_value_block_error_with_interface_payload() {
	for flag in [true, false] {
		_, item := match flag {
			true { wrapped_any_pair() }
			else { 2, unsafe { error('boom') } }
		}
		if _ := item {
			assert flag
		} else {
			assert !flag
		}
	}
}

fn test_match_tuple_direct_error_with_interface_payload() {
	for flag in [true, false] {
		_, item := match flag {
			true { wrapped_any_pair() }
			else { 2, error('boom') }
		}
		if _ := item {
			assert flag
		} else {
			assert !flag
			assert err.msg() == 'boom'
		}
	}
}

type ErrorOrText = IError | string

fn wrapped_error_or_text_pair() (int, !ErrorOrText) {
	return 1, ErrorOrText('ok')
}

fn test_match_tuple_direct_error_with_sum_payload() {
	for flag in [true, false] {
		_, item := match flag {
			true { wrapped_error_or_text_pair() }
			else { 2, error('boom') }
		}
		if _ := item {
			assert flag
		} else {
			assert !flag
			assert err.msg() == 'boom'
		}
	}
}

struct TupleConcreteError {
	reason string
}

fn (err TupleConcreteError) msg() string { return err.reason }

fn (_ TupleConcreteError) code() int { return 0 }

fn wrapped_concrete_error_pair() (int, !TupleConcreteError) {
	return 1, TupleConcreteError{'payload'}
}

fn test_tuple_value_block_failure_with_concrete_error_payload() {
	for flag in [true, false] {
		value, item := match flag {
			true { wrapped_concrete_error_pair() }
			else { 2, unsafe { error('match failure') } }
		}
		assert value == if flag { 1 } else { 2 }
		if got := item {
			assert flag
			assert got.reason == 'payload'
		} else {
			assert !flag
			assert err.msg() == 'match failure'
		}
		other_value, other_item := if flag {
			wrapped_concrete_error_pair()
		} else {
			3, unsafe { error('if failure') }
		}
		assert other_value == if flag { 1 } else { 3 }
		if got := other_item {
			assert flag
			assert got.reason == 'payload'
		} else {
			assert !flag
			assert err.msg() == 'if failure'
		}
	}
}

fn test_match_tuple_none_promotes_to_optional_slot() {
	for flag in [true, false] {
		value, text := match flag {
			true { wrapped_pair() }
			else { 2, none }
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
	result := os.exec([@VEXE, '-check', path])
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

fn test_if_tuple_failure_preserves_wrapper_kind() {
	for flag in [true, false] {
		value, text := if flag { wrapped_pair() } else { 2, none }
		assert value == if flag { 1 } else { 2 }
		assert (text or { 'failed' }) == if flag { 'ok' } else { 'failed' }

		result_value, result_text := if flag { wrapped_result_pair() } else { 3, error('x') }
		assert result_value == if flag { 1 } else { 3 }
		assert (result_text or { 'failed' }) == if flag { 'ok' } else { 'failed' }
	}
}

fn test_match_tuple_infers_option_from_none_and_value() {
	for flag in [true, false] {
		a, b := match flag {
			true { 1, none }
			else { 2, 'value' }
		}
		assert a == if flag { 1 } else { 2 }
		assert (b or { 'missing' }) == if flag { 'missing' } else { 'value' }
		c, d := match flag {
			true { 'value', 1 }
			else { none, 2 }
		}
		assert (c or { 'missing' }) == if flag { 'value' } else { 'missing' }
		assert d == if flag { 1 } else { 2 }
		e, f := if flag { 1, none } else { 2, 'value' }
		assert e == a
		assert (f or { 'missing' }) == (b or { 'missing' })
		g, h := if flag { 'value', 1 } else { none, 2 }
		assert (g or { 'missing' }) == (c or { 'missing' })
		assert h == d
	}
}

fn test_match_tuple_infers_each_optional_slot_across_all_arms() {
	for value in 0 .. 3 {
		a, b, c := match value {
			0 { none, 1, 'first' }
			1 { 'middle', none, 'second' }
			else { 'last', 3, none }
		}
		assert (a or { 'missing' }) == ['missing', 'middle', 'last'][value]
		assert (b or { 0 }) == [1, 0, 3][value]
		assert (c or { 'missing' }) == ['first', 'second', 'missing'][value]
		d, e := if value == 0 {
			1, none
		} else if value == 1 {
			2, 'x'
		} else {
			3, 'y'
		}
		assert d == value + 1
		assert (e or { 'missing' }) == ['missing', 'x', 'y'][value]
	}
}
