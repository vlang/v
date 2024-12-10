struct Foo {
	name string
}

type Foo1 = [2][2]int
type Foo2 = []int
type Foo3 = Foo

fn test_alias_compare_non_alias_value() {
	f1 := Foo1([[1, 2]!, [3, 4]!]!)
	assert f1 == [[1, 2]!, [3, 4]!]!
	assert [[1, 2]!, [3, 4]!]! == f1

	f2 := Foo2([1, 2, 3, 4])
	assert f2 == [1, 2, 3, 4]
	assert [1, 2, 3, 4] == f2

	f3 := Foo3(Foo{'hello'})
	assert f3 == Foo{'hello'}
	assert Foo{'hello'} == f3
}
