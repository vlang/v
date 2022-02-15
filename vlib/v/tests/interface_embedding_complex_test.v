fn test_interface_embedding_complex() {
	mut win := &Window{}
	ll := &LinearLayout{}
	win.initables << ll
	win.init()
}

//----------------------------------

[heap]
pub struct Window {
mut:
	initables []Initable
}

interface Initable {
	get_ptr()
mut:
	init(&Window)
}

fn (mut w Window) init() {
	for mut i in w.initables {
		i.init(w)
	}
}

//----------------------------------

pub struct ViewBase {}

pub fn (mut vb ViewBase) init(window &Window) {
	dump(@METHOD)
	assert false
}

pub fn (vb ViewBase) get_ptr() {}

//-------------------------------------

[heap]
pub struct ContainerBase {
	ViewBase
}

// want to excute this method
pub fn (mut cb ContainerBase) init(window &Window) {
	dump(@METHOD)
	assert true
}

//--------------------------------------

[heap]
pub struct LinearLayout {
	ContainerBase
}
