// vtest build: !msvc // error: cannot support compound statement expression ({expr; expr; expr;})
type Sum = int | string

struct Count {
mut:
	count int
}

fn (mut c Count) ret_sum() Sum {
	c.count++
	return c.count
}

fn test_sumtype_as_cast() {
	mut cnt := Count{22}
	_ := cnt.ret_sum() as int
	println(cnt)
	assert cnt.count == 23
}
