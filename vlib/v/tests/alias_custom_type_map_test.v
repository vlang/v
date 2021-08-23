import geometry

fn test_aliases_map_init() {
	a := geometry.ShapeMap({
		geometry.Shape.circle: 'Shape is a circle.'
	})
	assert a[geometry.Shape.circle] == 'Shape is a circle.'
}
