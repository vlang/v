module sample

pub const version = '1.0.0'
pub const count int = 42

pub fn hello(name string) string {
	return 'hi ' + name
}

pub fn generic[T](x T) T {
	return x
}

pub struct Point {
pub:
	x int
pub mut:
	y int
}

@[non_exhaustive]
pub enum Status {
	ready
	running
	failed
}

pub enum Color {
	red
	green
	blue
}

pub type MySum = int | string

pub type MyAlias = map[string]int

pub interface Greeter {
	greet(name string) string
}

pub fn (p Point) length() f64 {
	return 0.0
}
