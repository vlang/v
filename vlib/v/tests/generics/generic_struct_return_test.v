struct Thing[T] {
	count int
	items []T
}

fn new_thing[T](count int) Thing[T] {
	return Thing[T]{
		count: count
		items: []T{len: count}
	}
}

fn (t &Thing[T]) get(item int) T {
	return t.items[item]
}

fn specific(p int) int {
	thing := new_thing[int](2)
	thing.get(1)
	return p
}

fn generic[T](p T) int {
	thing := new_thing[f32](2)
	thing.get(1)
	return p
}

fn test_main() {
	r1 := generic(0)
	r2 := specific(0)
	assert r1 == r2
}
