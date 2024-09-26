enum MyEnum {
	something
	another
	third
}

type MyEnumAlias = MyEnum

fn test_enum_aliases() {
	x := MyEnum.something
	dump(x)
	a := MyEnumAlias.something
	dump(a)
	assert x == a

	dump(MyEnum.third)
	dump(MyEnumAlias.third)
	dump(int(MyEnum.third))
	dump(int(MyEnumAlias.third))
	assert MyEnum.third == MyEnumAlias.third
}
