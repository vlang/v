struct Foo {
	x f32
	y f32
}

pub fn (x Foo) == (y Foo) bool {
	return x.x == y.y
}

pub fn (x Foo) < (y Foo) bool {
	return x.x < y.x && x.y < y.y
}

pub fn (a Foo) + (b Foo) Foo {
	return Foo{a.x + b.x, a.y + b.y}
}

fn main() {
	x := Foo{4.0, 3.0}
	y := Foo{1.0, 2.0}
	println(x + y)
	println(Foo{42.42, 0.0} == Foo{0.0, 42.42})
	println(x > y)
}
