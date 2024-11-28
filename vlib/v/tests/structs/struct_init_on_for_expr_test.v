struct Iterator {
mut:
	counter int
}

fn (mut iter Iterator) next() ?int {
	if iter.counter > 0 {
		iter.counter--
		return iter.counter
	}
	return none
}

fn test_iterator_with_field_init() {
	for k, x in Iterator{
		counter: 10
	} {
		dump(x)
		if k == 0 {
			assert x == 9
		}
		if k == 9 {
			assert x == 0
		}
	}
}

//

struct OddNumberIterator {
mut:
	current i64
}

fn (mut i OddNumberIterator) next() ?i64 {
	i.current += 2
	return i.current + 1
}

fn test_iterator_without_field_init() {
	for x in OddNumberIterator{} {
		dump(x)
		if x > 10 {
			break
		}
	}
}
