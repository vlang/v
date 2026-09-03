struct MapRefShapes {
mut:
	rectangles []MapRefRectangle
}

struct MapRefRectangle {
mut:
	id     int
	square bool
}

fn (shapes MapRefShapes) get_squares() []&MapRefRectangle {
	return shapes.rectangles.filter(it.square).filter(it.id > 0).map(&it)
}

fn test_array_map_ref_it_after_filter_keeps_struct_fields() {
	mut shapes := MapRefShapes{}
	shapes.rectangles << MapRefRectangle{
		id:     1
		square: true
	}
	shapes.rectangles << MapRefRectangle{
		id:     2
		square: false
	}
	shapes.rectangles << MapRefRectangle{
		id:     3
		square: true
	}

	squares := shapes.get_squares()
	rendered := '${squares}'

	assert squares.len == 2
	assert voidptr(squares[0]) != voidptr(squares[1])
	assert squares[0].id == 1
	assert squares[0].square
	assert squares[1].id == 3
	assert squares[1].square
	assert rendered.contains('id: 1')
	assert rendered.contains('square: true')
}
