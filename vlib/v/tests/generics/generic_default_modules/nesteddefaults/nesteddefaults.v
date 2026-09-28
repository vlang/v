module nesteddefaults

pub struct Box[T] {
pub:
	value T = T{}
}

pub struct Wrapper[T] {
pub:
	box Box[T] = Box[T]{}
}

pub struct PointerBox[T] {
pub:
	value &T = &T{}
}
