module nested_cell_client

import nested_cell_mod

pub struct Box[T] {
pub:
	value T
}

pub struct Pair[T] {
pub:
	value T
}

pub struct Plain {
pub:
	value int
}

// run reports a nested cell type and its zero value.
pub fn run() string {
	cell := &nested_cell_mod.Cell[Box[Pair[int]]]{ value: Box[Pair[int]]{} }
	return nested_cell_mod.name[&Box[Pair[int]]]() + ' ' + cell.value.value.value.str()
}

// string_value reads a nested generic string cell.
pub fn string_value() string {
	cell := &nested_cell_mod.Cell[Box[Pair[string]]]{
		value: Box[Pair[string]]{ value: Pair[string]{ value: 'hello' } }
	}
	return cell.value.value.value
}

// controls exercises simpler generic cell arguments.
pub fn controls() int {
	pair := nested_cell_mod.Cell[Pair[int]]{ value: Pair[int]{ value: 3 } }
	plain := nested_cell_mod.Cell[Box[Plain]]{ value: Box[Plain]{ value: Plain{ value: 4 } } }
	return pair.value.value + plain.value.value.value
}
