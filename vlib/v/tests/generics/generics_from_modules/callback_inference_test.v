module main

import v.tests.generics.generics_from_modules.genericmodule

// Row has the name of genericmodule.Row: a placeholder inferred from callback,
// channel or fixed-array parameters keeps the caller's type, and so do the
// instantiated signatures and bodies that use it.
pub struct Row {
pub:
	id int
}

type RowVisitor = fn (Row) int

fn test_callback_parameter_infers_caller_type() {
	rows := [Row{
		id: 2
	}, Row{
		id: 3
	}]
	total := genericmodule.sum_rows(fn [rows] () []Row {
		return rows
	}, fn (cell &genericmodule.Cell[Row]) int {
		return cell.value.id
	})
	assert total == 5
}

fn test_channel_parameter_infers_caller_type() {
	values := chan Row{cap: 1}
	values <- Row{
		id: 7
	}
	first := genericmodule.first_of(values)
	assert typeof(first).name == 'Row'
	assert first.id == 7
}

fn test_fixed_array_parameter_infers_caller_type() {
	first := genericmodule.fixed_first([Row{
		id: 4
	}, Row{
		id: 5
	}]!)
	assert typeof(first).name == 'Row'
	assert first.id == 4
}

fn test_callback_result_parameter_infers_caller_type() {
	rows := [Row{
		id: 8
	}, Row{
		id: 9
	}]
	genericmodule.visit_each(fn [rows] () []Row {
		return rows
	}, fn (cell &genericmodule.Cell[Row]) ! {
		if cell.value.id == 9 {
			return error('row 9')
		}
	}) or {
		assert err.msg() == 'row 9'
		return
	}
	assert false
}

fn test_value_parameter_types_callback_with_caller_type() {
	total := genericmodule.visit_value(Row{ id: 6 }, fn (cell &genericmodule.Cell[Row]) int {
		return cell.value.id
	})
	assert total == 6
}

fn test_generic_application_keeps_caller_type() {
	cell := genericmodule.cell_of(Row{ id: 10 })
	assert cell.value.id == 10
	value := genericmodule.value_of(cell)
	assert typeof(value).name == 'Row'
	assert value.id == 10
	other := genericmodule.cell_of(genericmodule.Row{ name: 'module' })
	assert genericmodule.value_of(other).name == 'module'
}

fn test_receiver_method_callback_keeps_caller_type() {
	cell := genericmodule.cell_of(Row{ id: 11 })
	ids := cell.map_value(fn (rows []Row) []int {
		return rows.map(it.id)
	})
	assert ids == [11]
}

fn test_function_type_generic_argument_keeps_caller_type() {
	cell := genericmodule.cell_of(fn (row Row) Row {
		return Row{ id: row.id + 1 }
	})
	assert cell.value(Row{ id: 12 }).id == 13
	other := genericmodule.cell_of(fn (row genericmodule.Row) genericmodule.Row {
		return row
	})
	assert other.value(genericmodule.Row{ name: 'module' }).name == 'module'
}

fn test_channel_type_generic_argument_keeps_caller_type() {
	values := chan Row{cap: 1}
	values <- Row{ id: 14 }
	cell := genericmodule.cell_of(values)
	row := <-cell.value
	assert row.id == 14
}

fn test_nested_generic_argument_keeps_caller_type() {
	inner := genericmodule.cell_of(Row{ id: 15 })
	outer := genericmodule.cell_of(inner)
	assert outer.value.value.id == 15
}

fn test_callback_alias_generic_argument_keeps_caller_type() {
	visit := RowVisitor(fn (row Row) int {
		return row.id
	})
	cell := genericmodule.cell_of(visit)
	assert cell.value(Row{ id: 16 }) == 16
}

fn test_nested_callback_generic_argument_keeps_caller_type() {
	cell := genericmodule.cell_of(fn (visit fn (Row) Row, row Row) Row {
		return visit(row)
	})
	row := cell.value(fn (value Row) Row {
		return Row{ id: value.id + 1 }
	}, Row{ id: 17 })
	assert row.id == 18
}

fn test_mutable_callback_generic_argument_keeps_caller_type() {
	cell := genericmodule.cell_of(fn (mut rows []Row) int {
		rows << Row{ id: 19 }
		return rows.len
	})
	mut rows := []Row{}
	assert cell.value(mut rows) == 1
	assert rows[0].id == 19
}
