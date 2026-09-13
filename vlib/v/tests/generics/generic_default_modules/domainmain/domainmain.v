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
