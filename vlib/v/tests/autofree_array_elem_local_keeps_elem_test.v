// vtest vflags: -autofree -gc none
struct Column {
	name string
}

fn read_twice(columns []Column) []string {
	mut names := []string{}
	for _ in 0 .. 2 {
		column := columns[0]
		names << column.name.clone()
	}
	return names
}

fn test_local_copied_from_array_element_keeps_element() {
	columns := [Column{'v'.repeat(2)}]
	for _ in 0 .. 2 {
		for i in 1 .. 2 {
			column := columns[i - 1]
			assert column.name == 'vv'
		}
	}
	assert columns[0].name == 'vv'
}

fn test_parameter_element_is_not_moved_out() {
	columns := [Column{'v'.repeat(2)}]
	assert read_twice(columns) == ['vv', 'vv']
	assert columns[0].name == 'vv'
}

fn test_string_element_is_not_moved_out() {
	names := ['n'.repeat(2)]
	n := names[0]
	assert n == 'nn'
	assert names[0] == 'nn'
}
