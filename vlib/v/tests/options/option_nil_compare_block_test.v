struct Tag {
	id string
}

fn nil_tag() &Tag {
	return unsafe { nil }
}

// `nil` is only reachable in unsafe code, so comparing an option pointer against it is
// normally written as a block. The comparison lowers to a test on the option's `ok` field
// and never mentions the operand again, so anything else the block does has to be lowered
// separately or it is silently dropped.
fn test_a_nil_operand_block_still_runs_its_statements() {
	mut calls := []string{}
	empty := ?&Tag(none)

	assert empty == unsafe {
		calls << 'rhs'
		nil
	}
	assert calls == ['rhs']

	assert unsafe {
		calls << 'lhs'
		nil
	} == empty
	assert calls == ['rhs', 'lhs']

	assert empty != unsafe {
		calls << 'ne'
		nil
	} == false
	assert calls == ['rhs', 'lhs', 'ne']
}

fn test_a_nil_operand_block_compares_correctly_for_both_states() {
	empty := ?&Tag(none)
	present := ?&Tag(&Tag{ id: 'x' })
	mut calls := 0

	assert empty == unsafe {
		calls++
		nil
	}
	assert (present == unsafe {
		calls++
		nil
	}) == false
	assert present != unsafe {
		calls++
		nil
	}
	assert calls == 3, 'every operand has to be evaluated exactly once'
}

// The plain spelling must keep working, and must not evaluate anything extra.
fn test_a_plain_unsafe_nil_operand_still_compares() {
	empty := ?&Tag(none)
	present := ?&Tag(&Tag{ id: 'y' })
	assert empty == unsafe { nil }
	assert present != unsafe { nil }
	assert (present == unsafe { nil }) == false
	assert (empty != unsafe { nil }) == false
}

// The option side is the left operand here, so it has to be read before the block runs.
fn test_operand_evaluation_order_is_left_to_right() {
	mut order := []string{}
	assert read_option(mut order, ?&Tag(none)) == unsafe {
		order << 'block'
		nil
	}
	assert order == ['option', 'block']

	order = []
	assert unsafe {
		order << 'block'
		nil
	} == read_option(mut order, ?&Tag(none))
	assert order == ['block', 'option']
}

fn read_option(mut order []string, value ?&Tag) ?&Tag {
	order << 'option'
	return value
}

// A nested block still yields nil, and each level keeps its own statements.
fn test_nested_nil_operand_blocks_keep_every_statement() {
	mut calls := []string{}
	empty := ?&Tag(none)
	assert empty == unsafe {
		calls << 'outer'
		unsafe {
			calls << 'inner'
			nil
		}
	}
	assert calls == ['outer', 'inner']
}

// A bare `nil` returned from a function is unaffected by any of the above.
fn test_comparing_against_a_nil_returning_call() {
	empty := ?&Tag(none)
	assert empty == unsafe { nil }
	assert voidptr(nil_tag()) == unsafe { nil }
}
