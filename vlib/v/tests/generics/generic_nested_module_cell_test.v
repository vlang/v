import nested_cell_mod
import nested_cell_client

fn test_nested_module_generic_cell_keeps_type_owners() {
	assert nested_cell_client.run() == '&nested_cell_client.Box[nested_cell_client.Pair[int]] 0'
	assert nested_cell_client.string_value() == 'hello'
	assert nested_cell_client.controls() == 7
}

fn test_explicit_nested_module_types_remain_supported() {
	cell := nested_cell_mod.Cell[nested_cell_client.Box[nested_cell_client.Pair[int]]]{
		value: nested_cell_client.Box[nested_cell_client.Pair[int]]{
			value: nested_cell_client.Pair[int]{ value: 8 }
		}
	}
	assert cell.value.value.value == 8
}
