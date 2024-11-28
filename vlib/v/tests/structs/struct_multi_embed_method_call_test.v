pub struct Boundary {}

pub fn (b Boundary) contains(x int, y int) bool {
	return false
}

pub struct Base {
	Boundary
}

pub fn (mut b Base) on_event(x int, y int) {
	if b.Boundary.contains(x, y) {
	}
	if b.contains(x, y) {
	}
}

pub struct ListBox {
	Base
}

pub fn (mut lb ListBox) on_event(x int, y int) {
	if lb.Base.Boundary.contains(x, y) {
	}
	if lb.contains(x, y) {
	}
}

fn test_struct_multi_embed_method_call() {
	mut list_box := ListBox{}
	list_box.on_event(11, 22)
	assert true
}
