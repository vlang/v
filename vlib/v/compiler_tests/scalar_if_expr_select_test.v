// A plain scalar if-expression value is emitted as a C conditional. These
// cases check that it keeps V's typing and only evaluates the selected branch.

struct SelectProbe {
	id    int
	calls &int
}

fn (a SelectProbe) == (b SelectProbe) bool {
	unsafe {
		(*a.calls)++
	}
	return a.id == b.id
}

fn select_probe_locals(flag bool) (bool, int) {
	mut calls := 0
	a := &SelectProbe{
		id:    1
		calls: &calls
	}
	b := &SelectProbe{
		id:    1
		calls: &calls
	}
	x := if flag { true } else { a == b }
	return x, calls
}

fn select_probe_params(flag bool, a &SelectProbe, b &SelectProbe) bool {
	return if flag { true } else { a == b }
}

// Comparing struct pointers lowers to statements; they must stay in the
// branch that is selected, so the operator runs only when that branch does.
fn test_skipped_struct_pointer_equality_branch_is_not_evaluated() {
	taken, taken_calls := select_probe_locals(true)
	assert taken
	assert taken_calls == 0
	compared, compared_calls := select_probe_locals(false)
	assert compared
	assert compared_calls == 1
	mut calls := 0
	a := &SelectProbe{
		id:    1
		calls: &calls
	}
	b := &SelectProbe{
		id:    2
		calls: &calls
	}
	assert select_probe_params(true, a, b)
	assert calls == 0
	assert !select_probe_params(false, a, b)
	assert calls == 1
}

fn select_scaled(flag bool, x f32, y f32) f32 {
	return (if flag { 0.1 } else { x }) * y
}

fn select_wrapped(flag bool, x u8) u8 {
	return (if flag { x } else { 255 }) + 1
}

fn select_shift(flag bool, a u32, b u32) u32 {
	v := if flag { a << b } else { a >> b }
	return v
}

// The conditional is cast to the V result type, so C's arithmetic conversions
// do not widen it, and a shift in a branch value stays a shift.
fn test_scalar_select_keeps_v_types_and_operators() {
	assert select_scaled(true, 3, f32(0.3)) == f32(0.1) * f32(0.3)
	assert select_scaled(false, 3, f32(0.3)) == f32(3) * f32(0.3)
	assert select_wrapped(false, 7) == 0
	assert select_wrapped(true, 7) == 8
	assert select_shift(true, 3, 4) == 48
	assert select_shift(false, 48, 4) == 3
	values := [1, -2, 3, -4]
	mut clamped := []int{}
	for i in 0 .. values.len {
		value := values[i]
		positive := if value > 0 { value } else { 0 }
		clamped << positive
	}
	assert clamped == [1, 0, 3, 0]
}

fn check_scalar_select_mutable_iterator(flag bool) {
	mut values := [42, 84]
	for mut value in values {
		from_then := if flag { value } else { 0 }
		from_else := if flag { 0 } else { value }
		if flag {
			assert from_then == value
			assert from_else == 0
		} else {
			assert from_then == 0
			assert from_else == value
		}
	}
}

// Mutable iterator bindings use pointer storage, but a select reads their values.
fn test_scalar_select_reads_mutable_iterator_values() {
	check_scalar_select_mutable_iterator(true)
	check_scalar_select_mutable_iterator(false)
}

struct SelectOrderCell {
mut:
	value int
}

fn select_order_bump(mut cell SelectOrderCell) int {
	cell.value = 9
	return 1
}

fn check_scalar_select_operand_order(flag bool) {
	mut cell := SelectOrderCell{
		value: 1
	}
	expected := if flag { 2 } else { 1 }
	before_call := (if flag { cell.value } else { 0 }) + select_order_bump(mut cell)
	assert before_call == expected

	cell.value = 1
	before_if := (if flag { cell.value } else { 0 }) + (if flag {
		select_order_bump(mut cell)
	} else {
		1
	})
	assert before_if == expected

	cell.value = 1
	before_match := (if flag { cell.value } else { 0 }) + (match flag {
		true { select_order_bump(mut cell) }
		false { 1 }
	})
	assert before_match == expected

	cell.value = 1
	before_shift := (if flag { cell.value } else { 0 }) << (if flag {
		select_order_bump(mut cell)
	} else {
		1
	})
	assert before_shift == (if flag { 2 } else { 0 })
}

// A later operand can lower to statements that change a value read by the select.
fn test_scalar_select_precedes_later_operand_statements() {
	check_scalar_select_operand_order(true)
	check_scalar_select_operand_order(false)
}

fn test_scalar_select_condition_precedes_later_call() {
	mut cell := SelectOrderCell{
		value: 1
	}
	selected := (if cell.value == 1 { 1 } else { 0 }) + select_order_bump(mut cell)
	assert selected == 2
}
