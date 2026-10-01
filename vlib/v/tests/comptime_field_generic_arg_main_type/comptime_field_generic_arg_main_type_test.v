module main

import fieldreflect

// `fieldreflect` declares a private `Cell` too. The generic argument inferred from
// `value.$(field.name)` inside its generic bodies must stay this caller-owned `Cell`.
pub struct Cell {
pub:
	id   int
	name string
}

pub struct Demo {
pub:
	cells []Cell
	cell  Cell
	count int
}

fn test_comptime_field_array_arg_keeps_main_element_type() {
	d := Demo{}
	assert fieldreflect.array_field_element_names(d) == ['Cell']
	assert fieldreflect.array_field_element_fields(d) == ['id', 'name']
}

fn test_comptime_field_struct_arg_keeps_main_type() {
	assert fieldreflect.struct_field_type_names(Demo{}) == ['Cell']
}

pub struct Grid {
pub:
	cells []Cell
}

fn test_nested_specializations_keep_main_element_type() {
	grid := Grid{
		cells: [Cell{
			id:   1
			name: 'a'
		}, Cell{
			id:   2
			name: 'b'
		}]
	}
	assert fieldreflect.values_of(grid) == ['1', 'a', '2', 'b']
}

type Cells = []Cell
type CellAlias = Cell

pub struct Wrapped {
pub:
	by_name map[string]Cell
	maybe   ?Cell
	pair    [2]Cell
	list    Cells
	alias   CellAlias
}

fn test_comptime_field_composites_and_aliases_keep_main_type() {
	assert fieldreflect.field_type_names(Wrapped{}) == ['map[string]Cell', '?Cell', '[2]Cell',
		'Cells', 'CellAlias']
}

fn test_direct_calls_match_comptime_field_calls() {
	d := Demo{}
	assert fieldreflect.element_fields(d.cells) == ['id', 'name']
	assert fieldreflect.type_name(d.cell) == 'Cell'
	w := Wrapped{}
	assert fieldreflect.type_name(w.list) == 'Cells'
	assert fieldreflect.type_name(w.alias) == 'CellAlias'
	assert fieldreflect.local_cell_count() == 1
}
