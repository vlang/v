module mymod

pub const big_cap = 1000

pub struct Circle {
pub:
	r int
}

pub struct Square {
pub:
	s int
}

pub type Shape = Circle | Square

pub fn make_circle(r int) Shape {
	return Circle{r}
}

pub fn classify(s Shape) string {
	if s is Circle {
		return 'circle'
	}
	return 'other'
}

pub interface Speaker {
	speak() string
}

pub struct Dog {
	x int
}

pub fn (d Dog) speak() string {
	return 'woof'
}

pub fn make_speaker() Speaker {
	return Dog{1}
}

pub fn greet(s Speaker) string {
	return s.speak()
}

pub fn speaker_type(s Speaker) string {
	return typeof(s).name
}

pub fn make_getter(x int) fn () int {
	return fn [x] () int {
		return x
	}
}

pub fn call_getter_here(x int) int {
	g := make_getter(x)
	return g()
}

pub fn push_many() int {
	mut a := []int{cap: 2}
	for i in 0 .. 10 {
		a << i
	}
	return a.len
}
