module main

struct People[T] {
	raw T
}

struct Man {
	name string
}

fn test_main() {
	m1 := Man{
		name: 'Tom'
	}
	gen1(m1)
}

fn gen1[T](m T) {
	gen2[People[T]]()
}

fn gen2[T]() {
}
