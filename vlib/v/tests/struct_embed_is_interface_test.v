pub struct Base {
mut:
	x int
	y int
}

pub fn (mut b Base) init() {}

pub fn (b Base) get_pos() (int, int) {
	return b.x, b.y
}

pub struct Label {
	Base
}

pub interface Widget {
mut:
	init()
}

pub interface Layoutable {
	get_pos() (int, int)
}

pub struct Layout {
	Base
mut:
	widgets []Widget
}

pub fn (mut l Layout) layout() {
	for wd in l.widgets {
		if wd is Layoutable {
			lw := wd as Layoutable
			dump(lw.get_pos())
			x, y := lw.get_pos()
			assert x == 10
			assert y == 20
		} else {
			println('wd is NOT Layoutable')
		}
	}
}

fn test_struct_embed_is_interface() {
	mut l := Layout{}
	l.widgets << Label{
		x: 10
		y: 20
	}
	l.layout()
}
