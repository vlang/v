import v.eval

fn test_const() {
	mut e := eval.create()

	ret := e.run('const a = 100
	const b = "test"
	const c = if a == 100 { 666 } else { 999 }
	fn display() (int,string,int) { println(a) println(b) println(c) return a,b,c } display()')!

	dump(ret)
	assert ret[0].int_val() == 100
	assert ret[1].string() == 'test'
	assert ret[2].int_val() == 666
}
