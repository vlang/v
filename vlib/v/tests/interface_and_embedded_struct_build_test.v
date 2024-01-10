pub struct Event {}

pub struct ViewBase {}

pub fn (vb ViewBase) point_inside(e Event) {}

pub struct ContainerBase {
	ViewBase
}

interface Focusable {
	point_inside(Event)
}

fn test_interface_and_embedded_struct_build() {
	assert true
}
