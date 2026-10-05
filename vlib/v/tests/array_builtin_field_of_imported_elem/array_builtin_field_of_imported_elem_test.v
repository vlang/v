module main

import rows

// `.data` and `.cap` on a `[]rows.Row` are the array's own fields, not the
// element struct's same-named fields.
fn test_array_fields_on_an_array_of_an_imported_struct() {
	a := []rows.Row{len: 2, cap: 5}
	b := a
	assert a.data == b.data
	assert a.cap == 5
	c := []rows.Row{len: 2}
	assert a.data != c.data
}

fn test_array_fields_inside_the_module_that_declares_the_element() {
	l := rows.Log{
		rows: []rows.Row{len: 2, cap: 7}
	}
	m := rows.Log{
		rows: l.rows
	}
	assert l.shares_rows_with(m)
	assert !l.shares_rows_with(rows.Log{
		rows: []rows.Row{len: 2}
	})
	assert l.row_capacity() == 7
}
