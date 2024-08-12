type MyType = int | string
type MyType2 = bool | int | string
type MyType3 = string | u64
type MyType4 = ?string | bool

fn test_main() {
	x := MyType{}
	println(x)
	assert x == MyType(0)

	y := MyType2{}
	println(y)
	assert y == MyType2(false)

	w := MyType3{}
	println(w)
	assert w == MyType3('')

	z := MyType4{}
	println(z)
	assert z == MyType4(?string(none))
}
