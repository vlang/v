struct Context[U] {
	name string
	x    U
}

type Handler[U] = fn (ctx Context[U]) ?

fn f1[U](ctx Context[U]) ? {}

fn f2[U](ctx Context[U]) ? {}

// Keep these unused specialized signatures distinct; issue #23014 failed in cgen here.
fn f3(ctx Context[int]) ? {}

fn f4(ctx Context[u8]) ? {}

struct App[U] {
	x U
}

fn (mut app App[U]) get(prefix string, handlers []Handler[U]) {
	assert prefix.len > 0
	assert handlers.len == 2
}

fn test_generics_unused_specialized_fn_array_param() {
	mut app := App[u64]{}
	app.get('abc', [f1[u64], f2[u64]])
	assert true
}
