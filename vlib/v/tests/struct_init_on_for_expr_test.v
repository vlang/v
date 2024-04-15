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

fn test_main() {
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
