fn JS.Math.pow(JS.Number, JS.Number) JS.Number

fn test_js_prim_cast() {
	x := JS.Number(f64(42.42))
	assert f64(x) == 42.42
	y := JS.BigInt(u64(18446744073709551615))
	assert u64(y) == u64(18446744073709551615)
	z := JS.String('hello, world!')
	assert string(z) == 'hello, world!'
	w := int(JS.Math.pow(JS.Number(int(2)), JS.Number(int(3))))
	assert w == 8
}
