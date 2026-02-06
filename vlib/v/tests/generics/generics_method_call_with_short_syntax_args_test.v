struct Options[T] {
	a T
}

struct Collector[T] {
mut:
	a T
}

fn (mut c Collector[T]) use(options Options[T]) {
	c.a = options.a
}

struct MainStruct {
	Collector[int]
}

fn test_main() {
	mut s := MainStruct{}
	s.use(a: 1)
	assert s.a == 1
}
