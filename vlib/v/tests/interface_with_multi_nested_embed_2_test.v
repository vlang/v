fn test_interface_with_multi_nested_embed() {
	mut ll := &LinearLayout{}
	mut lbl := &Label{
		x: 10
		y: 20
	}
	ll.add(mut lbl)

	println(ll)

	assert ll.x == 10
	assert ll.y == 20
}

pub struct Rect {
mut:
	x int
	y int
}

pub fn (r Rect) get_pos() (int, int) {
	return r.x, r.y
}

pub struct LayoutBase {
	Rect
}

pub struct Base {
	LayoutBase
}

pub fn (mut b Base) init() {}

[heap]
pub struct Label {
	Base
}

pub interface Layoutable {
	get_pos() (int, int)
mut:
	init()
}

[heap]
pub struct LinearLayout {
	Base
mut:
	layoutables []Layoutable
}

pub fn (mut ll LinearLayout) add(mut l Layoutable) {
	x, y := l.get_pos()
	ll.x += x
	ll.y += y
}
