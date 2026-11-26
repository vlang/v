// vtest vflags: -autofree -gc none
struct Table {
	name string
	cols []string
}

fn test_for_in_array_literal_of_locals_keeps_locals() {
	left := ['a'.repeat(2), 'b'.repeat(2)]
	right := ['c'.repeat(2)]
	mut lens := []int{}
	for row in [left, right] {
		lens << row.len
	}
	// Reuse the freed memory, if any, before reading the locals again.
	other := ['x'.repeat(2), 'y'.repeat(2), 'z'.repeat(2)]
	assert lens == [2, 1]
	assert left == ['aa', 'bb']
	assert right == ['cc']
	assert other.len == 3
}

fn test_array_literal_of_struct_locals_keeps_locals() {
	a := Table{'a'.repeat(2), ['x'.repeat(2)]}
	b := Table{'b'.repeat(2), []}
	{
		rows := [a, b]
		assert rows.len == 2
		assert rows[0].cols == ['xx']
	}
	assert a.name == 'aa'
	assert a.cols == ['xx']
	assert b.name == 'bb'
}
