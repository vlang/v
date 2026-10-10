fn anonymous_array_number() int {
	return 7
}

fn test_inferred_anonymous_rows_preserve_array_storage() {
	row := struct { item: anonymous_array_number() }
	other := struct { item: anonymous_array_number() * 2 }
	rows := [row, other]
	assert rows.len == 2
	assert rows.element_size == int(sizeof(row))
	assert unsafe { &int(rows.data)[0] } == 7
	assert unsafe { &int(rows.data)[1] } == 14
	copied := rows.clone()
	assert copied.len == 2
	assert copied.element_size == rows.element_size
	assert unsafe { &int(copied.data)[1] } == 14
}

fn test_inferred_anonymous_literals_preserve_array_storage() {
	rows := [struct { item: anonymous_array_number() }]
	assert rows.len == 1
	assert rows.element_size == int(sizeof(int))
	assert unsafe { &int(rows.data)[0] } == 7
}

fn test_nested_inferred_anonymous_arrays_preserve_storage() {
	row := struct { item: anonymous_array_number() }
	rows := [[row], [row]]
	assert rows.len == 2
	assert rows[0].len == 1
	assert rows[1].element_size == int(sizeof(row))
	assert unsafe { &int(rows[1].data)[0] } == 7
}
