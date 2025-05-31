type Foo = fn (bar string)

fn test_main() {
	mut func := ?Foo(none)
	res := dump(func)
	assert res == none

	mut func2 := ?Foo(fn (bar string) {})
	res2 := dump(func2)
	assert res2 != none
	assert res2?.str() == 'fn (string)'
}
