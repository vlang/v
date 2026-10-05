module shapes

pub type Shape = Circle(f64) | Square(f64) | Nothing

pub fn describe(s Shape) string {
	return match s {
		Shape.Circle(r) { 'circle ${r}' }
		Shape.Square(a) { 'square ${a}' }
		Shape.Nothing { 'nothing' }
	}
}

pub fn unit() Shape {
	return Shape.Circle(1.0)
}
