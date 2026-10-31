// Each nested `or {}` / `else {}` block binds its own `err`, so a smartcast of the
// outer `err` must not apply to it, and smartcasts made inside must not leak out.
// See https://github.com/vlang/v/issues/28824
struct NestedOrErrA {
	Error
	code int
}

struct NestedOrErrB {
	Error
	code int
}

struct NestedOrLog {
mut:
	items []string
}

fn (mut l NestedOrLog) add(item string) {
	l.items << item
}

fn nested_or_fail_a() ! {
	return NestedOrErrA{
		code: 1
	}
}

fn nested_or_fail_b() !int {
	return NestedOrErrB{
		code: 2
	}
}

fn nested_or_fail_pair() !(int, int) {
	return NestedOrErrB{
		code: 5
	}
}

fn test_same_error_type_in_nested_or_blocks() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		assert err is NestedOrErrA
		nested_or_fail_a() or {
			assert err is NestedOrErrA
			log.add('ok ${err.code}')
		}
	}
	assert log.items == ['ok 1']
}

fn test_statement_or_block_inside_smartcast_err() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		if err is NestedOrErrA {
			nested_or_fail_b() or {
				if err is NestedOrErrB {
					log.add('inner ${err.code}')
				}
				0
			}
			log.add('outer ${err.code}')
		}
	}
	assert log.items == ['inner 2', 'outer 1']
}

fn test_value_or_block_inside_smartcast_err() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		if err is NestedOrErrA {
			x := nested_or_fail_b() or {
				if err is NestedOrErrB {
					err.code * 10
				} else {
					-1
				}
			}
			log.add('${x} ${err.code}')
		}
	}
	assert log.items == ['20 1']
}

fn test_if_guard_else_inside_smartcast_err() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		if err is NestedOrErrA {
			if v := nested_or_fail_b() {
				log.add('value ${v}')
			} else {
				if err is NestedOrErrB {
					log.add('inner ${err.code}')
				}
			}
			log.add('outer ${err.code}')
		}
	}
	assert log.items == ['inner 2', 'outer 1']
}

fn test_multi_return_or_block_inside_smartcast_err() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		if err is NestedOrErrA {
			a, b := nested_or_fail_pair() or {
				if err is NestedOrErrB {
					log.add('inner ${err.code}')
				}
				7, 8
			}
			log.add('${a} ${b} ${err.code}')
		}
	}
	assert log.items == ['inner 5', '7 8 1']
}

fn test_assert_smartcasts_in_nested_or_blocks() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		assert err is NestedOrErrA
		nested_or_fail_b() or {
			assert err is NestedOrErrB
			log.add('inner ${err.code}')
			0
		}
		log.add('outer ${err.code}')
	}
	assert log.items == ['inner 2', 'outer 1']
}

fn test_inner_smartcast_does_not_leak_to_outer_err() {
	mut log := NestedOrLog{}
	nested_or_fail_a() or {
		nested_or_fail_b() or {
			assert err is NestedOrErrB
			0
		}
		if err is NestedOrErrA {
			log.add('outer ${err.code}')
		}
	}
	assert log.items == ['outer 1']
}
