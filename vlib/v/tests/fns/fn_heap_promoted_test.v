@[heap]
struct Context {}

type Method = fn (ctx Context)

fn call(method Method, ctx Context) { // or switch `ctx` and `method` also ok
	method(ctx)
}

fn get(ctx Context) {
	println('ok')
}

fn test_main() {
	call(get, Context{})
	assert true
}
