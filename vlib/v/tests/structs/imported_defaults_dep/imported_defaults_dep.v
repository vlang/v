module imported_defaults_dep

pub struct Color {
pub:
	r u8
	g u8
	b u8
}

pub enum Direction {
	horizontal
	vertical
}

pub struct Scrollbar {
pub:
	direction Direction
	width     int
}

// Scrollbar.create returns a scrollbar for one axis.
pub fn Scrollbar.create(direction Direction) Scrollbar {
	return Scrollbar{
		direction: direction
		width:     7
	}
}

@[heap]
pub struct StyleValue {
pub:
	n int
}
