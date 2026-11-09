// package: example:demo
// version: 0.1.0

// interface types
pub struct Point {
	x f64
	y f64
}
pub enum Color {
	red
	green
	blue
}
pub struct Shape_Circle {
	value f64
}
pub struct Shape_Empty {}
pub type Shape = Shape_Circle | Shape_Empty
pub type Perms = int
pub const perms_read = 1
pub const perms_write = 2
pub type Alias = u32