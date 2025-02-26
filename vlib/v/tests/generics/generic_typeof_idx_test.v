type MyInt = int
type MyString = string

struct Foo {
	a []int
	b MyInt
	c MyString
}

fn unaliased_typ[T](a T) int {
	$if typeof[T]().unaliased_typ is $int {
		return 1
	} $else $if typeof[T]().unaliased_typ is $string {
		return 2
	}
	return 0
}

fn idx[T](a [][]T) int {
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
	assert idx([a.a]) == 1
	assert idx([['']]) == 2

	assert unaliased_typ(1) == 1
	assert unaliased_typ('') == 2
}
