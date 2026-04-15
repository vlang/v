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

	squares := shapes.get_squares()
	rendered := '${squares}'

	assert squares.len == 1
	assert squares[0].id == 1
	assert squares[0].square
	assert rendered.contains('id: 1')
	assert rendered.contains('square: true')
}
