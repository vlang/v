pub struct Boundary {
mut:
	x        int
	y        int
	offset_x int
	offset_y int
	width    int
	height   int
}

pub struct WidgetBase {
	Boundary
mut:
	id      string
	z_index int
	hidden  bool
}

pub struct Label {
	WidgetBase
}

fn test_multiple_embed_struct_init() {
	l := Label{
		x: 100
		y: 200
	}
	println(l)
	assert l.x == 100
	assert l.y == 200
}
