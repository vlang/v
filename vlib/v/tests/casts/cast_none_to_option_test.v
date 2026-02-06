struct Test {}

type AliasType = Test

type I20 = int

type Byt = u8

fn test_cast_none() {
	a := ?I20(none)
	assert a == none
	b := ?int(none)
	assert b == none
	c := ?i16(none)
	assert c == none
	d := ?u32(none)
	assert d == none
	e := ?u16(none)
	assert e == none
	f := ?u8(none)
	assert f == none
	g := ?Test(none)
	assert g == none
	h := ?AliasType(none)
	assert h == none
	i := ?Byt(none)
	assert i == none
	j := ?rune(none)
	assert j == none
	k := ?string(none)
	assert k == none
	l := ?[]Byt(none)
	assert l == none
	m := ?[]Test(none)
	assert m == none
	n := ?[]AliasType(none)
	assert n == none
}
