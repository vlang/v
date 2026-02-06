import v.eval

fn test_pushval() {
	mut e := eval.create()

	e.push_val(i64(2))
	e.push_val(f64(2.2))
	e.push_val('calc')

	ret := e.run('println("\${host_pop()}: \${i64(host_pop())+i64(host_pop())}")')!
	assert ret == []
}

fn test_ret() {
	mut e := eval.create()

	e.push_val(i64(2))
	e.push_val(f64(2.2))
	e.push_val('calc')

	ret := e.run('fn test() string { return "\${host_pop()}: \${i64(host_pop())+i64(host_pop())}" } test()')!
	assert ret[0].string() == 'calc: 4'
}

fn test_reuse_stack() {
	mut e := eval.create()

	e.push_val(i64(1))
	e.push_val(i64(2))

	e.push_val(i64(3))
	e.push_val(i64(4))

	code := 'fn calc(x int, y int) int { return x + y } calc(host_pop(), host_pop())'

	ret := e.run(code)!
	assert ret[0].string() == '7'

	ret2 := e.run(code)!
	assert ret2[0].string() == '3'
}
