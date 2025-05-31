struct Foo {
	a int
}

type Bar = Foo

type ArrBar = []Bar

fn test_main() {
	assert typeof[Bar]().unaliased_typ == typeof[Foo]().unaliased_typ
	assert typeof[int]().unaliased_typ != typeof[Foo]().unaliased_typ
	assert typeof[int]().unaliased_typ == typeof[int]().unaliased_typ
	assert typeof[ArrBar]().unaliased_typ == typeof[[]Bar]().idx

	a := Bar{}
	assert typeof(a).unaliased_typ == typeof[Foo]().idx

	b := ArrBar{}
	assert typeof(b).unaliased_typ == typeof[[]Bar]().idx
}
