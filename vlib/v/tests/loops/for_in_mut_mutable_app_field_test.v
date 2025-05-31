struct Item {
mut:
	field []int
}

struct App {
mut:
	index [][]Item
}

fn test_for_in_struct_with_mutable_array_field_indexed_several_times_in_the_loop_condition_part() {
	mut app, x, y := App{[[Item{
		field: [1, 2, 3]
	}]]}, 0, 0
	for i, mut v in app.index[x][y].field {
		assert v != 0
		v = 555
	}
	assert app.index[x][y] == Item{[555, 555, 555]}
}
