enum Test {
	abc
}

type SumTest = Test | u8

type TestAlias = Test

fn test_main() {
	a := TestAlias.abc
	b := SumTest(a)
	assert b is Test
}
