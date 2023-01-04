fn test_interface_embedding_method_call() {
	mut window := &Window{}
	btn := &Button{}
	window.initables << btn
	window.run()
}

[heap]
pub struct Window {
mut:
	initables []Initable
	popview   PopView = DummyPopup{}
}

interface Initable {
mut:
	init(&Window)
}

interface Drawable {
	draw()
}

pub fn (mut window Window) run() {
	for mut i in window.initables {
		i.init(window)
	}
	for wd in window.initables {
		if wd is Drawable {
			d := wd as Drawable
			d.draw()
		}
	}
	window.popview.draw()
}

struct DummyPopup {}

fn (d DummyPopup) draw() {}

interface PopView {
	Drawable
}

[heap]
pub struct Button {
mut:
	window &Window = unsafe { nil }
}

pub fn (mut b Button) init(window &Window) {
	b.window = window
}

pub fn (b Button) draw() {
	g := b.window.initables
	println(g.len)
	assert g.len == 1
}
