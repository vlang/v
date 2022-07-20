fn test_interface_with_multi_nested_embed() {
	mut win := &Window{}
	mut ll := &LinearLayout{}
	mut lbl := &Label{
		x: 10
		y: 20
	}

	ll.add(mut lbl)
	win.add(mut ll)

	win.init()
}

[heap]
pub struct Window {
mut:
	initables []&Initable
}

interface Initable {
mut:
	init(&Window)
}

pub fn (mut w Window) add(mut initable Initable) {
	w.initables << initable
}

interface Container {
mut:
	layout()
}

fn (mut w Window) init() {
	for wd in w.initables {
		if wd is Container {
			mut c := unsafe { wd }
			c.layout()
		}
	}
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

pub fn (mut b Base) init(window &Window) {}

[heap]
pub struct Label {
	Base
}

pub interface Layoutable {
	get_pos() (int, int)
}

[heap]
pub struct LinearLayout {
	Base
mut:
	layoutables []Layoutable
}

pub fn (mut ll LinearLayout) add(mut l Layoutable) {
	ll.layoutables << l
}

pub fn (mut ll LinearLayout) layout() {
	for mut wl in ll.layoutables {
		x, y := wl.get_pos()
		println('$x, $y')
		assert x == 10
		assert y == 20
	}
}
