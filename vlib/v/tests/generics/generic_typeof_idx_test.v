struct Foo {
	a []int
}

fn t[T](a [][]T) int {
	$if typeof[T]().idx is $int {
		return 1
	} $else $if typeof[T]().idx is $string {
		return 2
	}
	return 0
}

fn test_main() {
	a := Foo{
		a: [1, 2, 3]
	}
	assert t([a.a]) == 1
	assert t([['']]) == 2
}
