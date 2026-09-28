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

// The operand's statements keep the block scope they were written in. Splicing them into the
// enclosing scope instead put a block-local declaration one level up, so an outer variable of
// the same name became a second declaration in one C scope and the program stopped compiling.
fn test_a_nil_operand_block_keeps_its_own_scope() {
	empty := ?&Tag(none)
	mut seen := []int{}

	assert empty == unsafe {
		local := 1
		seen << local
		nil
	}
	// Declaring the same name in the enclosing scope afterwards has to stay legal.
	local := 2
	assert local == 2
	assert seen == [1]
}

fn test_a_nil_operand_block_scope_is_kept_in_both_operand_positions() {
	empty := ?&Tag(none)
	mut seen := []int{}

	assert unsafe {
		shared_name := 10
		seen << shared_name
		nil
	} == empty
	assert empty != unsafe {
		shared_name := 20
		seen << shared_name
		nil
	} == false

	shared_name := 30
	assert shared_name == 30
	assert seen == [10, 20]
}

// Each nested level is its own scope, so the same name may be declared at every one of them.
fn test_nested_nil_operand_blocks_each_keep_their_scope() {
	empty := ?&Tag(none)
	mut seen := []int{}

	assert empty == unsafe {
		depth := 1
		seen << depth
		unsafe {
			depth := 2
			seen << depth
			nil
		}
	}
	depth := 3
	assert depth == 3
	assert seen == [1, 2]
}

// A block-local must not outlive its operand: the value the comparison yields still comes
// from the option, and the statements run for their effect only.
fn test_a_nil_operand_block_local_does_not_leak_into_the_result() {
	present := ?&Tag(&Tag{ id: 'x' })
	mut calls := 0
	assert (present == unsafe {
		ignored := 99
		calls += ignored - 98
		nil
	}) == false
	assert calls == 1
}
