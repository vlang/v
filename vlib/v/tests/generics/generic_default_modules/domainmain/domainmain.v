module domainmain

pub struct GenericBox[T] {
pub mut:
	x    int = 7
	size int = sizeof(T)
	ch   chan T = chan T{cap: 1}
}

pub struct Local {
pub:
	x int = 22
}

pub struct Box[T] {
pub:
	value T = T{}
}

pub interface Shape {
	area() int
}

pub struct Square {
pub:
	side int
}

pub fn (s Square) area() int {
	return s.side * s.side
}

pub struct Circle {
pub:
	radius int
}

pub fn (c Circle) area() int {
	return 3 * c.radius * c.radius
}

pub const shape_value = Shape(Square{
	side: 2
})

pub struct ShapeHolder[T] {
pub:
	matches bool = shape_value is T
}

pub type Sum = int | string

pub const sum_value = Sum(5)

pub struct SumHolder[T] {
pub:
	matches bool = sum_value is T
}

pub struct AsHolder[T] {
pub:
	value T = sum_value as T
}
