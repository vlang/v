struct Foo {}

type Sum = int | string | Foo
type SumAlias = Sum

fn test_struct() {
	a := SumAlias(Foo{})
	assert '${a}' == 'SumAlias(Sum(Foo{}))'
}

fn test_int() {
	a := SumAlias(0)
	assert '${a}' == 'SumAlias(Sum(0))'
}

fn test_string() {
	a := SumAlias('foo')
	assert '${a}' == "SumAlias(Sum('foo'))"
}
