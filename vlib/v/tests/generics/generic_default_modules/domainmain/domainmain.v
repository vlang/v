module domainmain

pub struct GenericBox[T] {
pub mut:
	x    int    = 7
	size int    = sizeof(T)
	ch   chan T = chan T{cap: 1}
}

// Local collides with the test's own `Local`; a caller's `Local` argument must
// not resolve to this one inside the defaults of `Described`.
pub struct Local {
pub:
	x int = 22
}

pub struct Described[T] {
pub:
	size  int    = sizeof(T)
	name  string = T.name
	tname string = typeof[T]().name
	n     int    = 3
}
