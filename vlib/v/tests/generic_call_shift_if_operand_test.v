struct Column {
	from i32
}

fn column_at[T](base &T, index isize) &T {
	return unsafe { base + index }
}

// A branch tail that shifts by a field of an inferred generic call is a value,
// also where the `if` is an operand of a compound assignment.
fn test_if_operand_with_generic_call_shift_tail() {
	columns := [Column{3}, Column{40}]
	mut mask := u32(0)
	for i in 0 .. 2 {
		mask |= (if column_at(&columns[0], i).from > 31 {
			u32(0x80000000)
		} else {
			u32(1) << column_at(&columns[0], i).from
		})
	}
	assert mask == u32(0x80000008)
}

fn test_if_operand_with_parenthesized_generic_call_shift_tail() {
	columns := [Column{3}, Column{40}]
	mut mask := u32(0)
	for i in 0 .. 2 {
		mask |= (if column_at(&columns[0], i).from > 31 {
			u32(0x80000000)
		} else {
			(u32(1) << column_at(&columns[0], i).from)
		})
	}
	assert mask == u32(0x80000008)
}
