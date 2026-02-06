module zmod_foo

pub struct Foo[T] {}

pub fn Foo.new[T](embed T) &Foo[T] {
	return &Foo[T]{}
}
