type Sum = int | string
type SumAlias = Sum

fn test_alias_sumtype_smartcast() {
	a_int := SumAlias(Sum(10))
	if a_int is int {
		assert a_int == 10
	}

	a_str := SumAlias('foo')
	if a_str is string {
		assert a_str == 'foo'
	}
}
