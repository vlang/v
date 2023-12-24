@[heap]
struct UI {}

pub fn (ui &UI) draw_rect() {
	println('[]')
}

@[heap]
pub interface Node {
	id u64
	draw()
mut:
	ui &UI
	init(ui &UI) !
}

@[heap]
pub struct Item {
pub:
	id u64 = 1
mut:
	ui   &UI = unsafe { nil }
	body []&Node
}

pub fn (mut i Item) init(ui &UI) ! {
	assert i != unsafe { nil } // This assert generates a C gen error
	i.ui = ui
	for mut child in i.body {
		child.init(ui)!
	}
}

pub fn (i &Item) draw() {
	assert i != unsafe { nil }
	for child in i.body {
		child.draw()
	}
}

@[heap]
pub struct Rectangle {
	Item
pub mut:
	field f32
}

pub fn (r &Rectangle) draw() {
	assert r != unsafe { nil }
	r.ui.draw_rect()
	r.Item.draw()
}

fn test_infix_expr_in_mut_receiver_method() {
	ui := &UI{}
	mut rect := &Rectangle{}

	rect.init(ui)!

	rect.draw()
	assert true
}
