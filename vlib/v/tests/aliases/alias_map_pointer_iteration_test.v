type PointerTable = &map[string]int

type ChainedPointerTable = PointerTable

type PointerNumber = &int

fn pointer_table_total(table PointerTable) int {
	mut total := 0
	for _, value in *table {
		total += value
	}
	return total
}

fn chained_pointer_table_values(table ChainedPointerTable) map[string]int {
	mut values := map[string]int{}
	for key, value in *table {
		values[key] = value
	}
	return values
}

fn pointer_number_value(value PointerNumber) int {
	return unsafe { *value }
}

fn test_alias_map_pointer_iteration_keeps_the_dereference() {
	table := PointerTable(&map[string]int{
		'a': 2
		'b': 3
	})
	assert pointer_table_total(table) == 5
	assert pointer_table_total(PointerTable(&map[string]int{})) == 0
	assert chained_pointer_table_values(ChainedPointerTable(table)) == {
		'a': 2
		'b': 3
	}
}

fn test_alias_scalar_pointer_parameter_keeps_the_dereference() {
	value := 42
	assert pointer_number_value(PointerNumber(&value)) == 42
}
