module samename

pub struct Foo {
pub:
	a string
}

pub struct Box[T] {
pub:
	v T
}

pub enum Color {
	red
	green
}

pub type Num = i64

pub interface Speaker {
	speak() string
}

pub type Sum = Foo | int

pub fn (f Foo) speak() string {
	return 'mod ${f.a}'
}
