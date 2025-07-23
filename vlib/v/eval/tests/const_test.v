import v.eval

fn test_const() {
	mut e := eval.create()

	ret := e.run('const a = 100
	const b = "test"
	fn display() (int,string) { println(a) println(b) return a,b } display()')!

	dump(ret)
	assert ret[0].int_val() == 100
	assert ret[1].string() == 'test'
}
