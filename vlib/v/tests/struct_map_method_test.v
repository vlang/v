type Foo = int

fn (a Foo) map(add int) string {
	return (a + add).str()
}

fn test_map_one_arg() {
	a := Foo(0)
	assert a.map(1) == '1'
	assert Foo(3).map(3) == '6'
}

type Bar = int

fn (b Bar) map() int {
	return b + 1
}

fn test_map_no_arg() {
	b := Bar(0)
	assert b.map() == 1
	assert Bar(1).map() == 2
}

type Baz = int

fn (b Baz) map(a int, c int) int {
	return b + (a - c)
}

fn test_map_more_args() {
	b := Baz(0)
	assert b.map(5, 2) == 3
	assert Baz(3).map(2, 5) == 0
}
