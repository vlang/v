struct Aaa {
pub mut:
	v []int
}

struct Bbb {
pub mut:
	a []Aaa
}

fn foo(b int, mut a []int) {
	a[0] = 7
	// a << 4
}

fn test_mut() {
	mut numbers := [1, 2, 3]
	foo(7, mut numbers)
	assert numbers.len == 3
	// TODO bring back once << works with mutable args
	// assert numbers.len == 4
	// assert numbers[0] == 7
	// assert numbers[3] == 4
	println(numbers)
	n := 1
	mut b := (&n)
	//
	(*b) = 10
	// mut b := mut a
	// b = 10
}

fn test_mut_2() {
	zero := 0
	mut b := Bbb{}
	b.a << Aaa{}
	b.a[0].v = [9, 8, 7]
	b.a[0].v << 6
	b.a[zero].v << 5
	b.a[0].v[zero] = 3
	b.a[0].v[b.a[zero].v[zero]] += 2 - 1 // TODO
	b.a[0].v[b.a[0].v[zero]] += 2 - 1 // TODO
	assert b.a[0].v.len == 5
	assert b.a[0].v[0] == 3
	assert b.a[0].v[1] == 8
	assert b.a[0].v[2] == 7
	assert b.a[0].v[3] == 8
	assert b.a[0].v[4] == 5
}
