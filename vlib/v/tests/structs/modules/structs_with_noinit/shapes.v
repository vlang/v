module structs_with_noinit

@[noinit]
pub struct Image {}

@[noinit]
pub struct Rect {}

@[noinit]
pub struct Circle {}

pub fn make_circle() Circle {
	return Circle{}
}

pub fn make_image() Image {
	return Image{}
}

pub fn make_rect() Rect {
	return Rect{}
}
