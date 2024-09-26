struct Context {}

pub fn (mut ctx Context) default_route() string {
	println('from Context')
	return 'from Context'
}

struct App {
	Context
}

struct Other {
	Context
}

pub fn (mut app Other) default_route() string {
	println('from Other')
	return 'from Other'
}

fn test_generic_method_on_nested_struct() {
	mut app := &App{}
	ret1 := call_generic(mut app)
	assert ret1 == 'from Context'
	mut other := &Other{}
	ret2 := call_generic(mut other)
	assert ret2 == 'from Other'
}

fn call_generic[T](mut app T) string {
	return app.default_route()
}
