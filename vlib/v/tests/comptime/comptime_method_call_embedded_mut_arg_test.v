struct Base {
mut:
	n int
}

struct Ctx {
	Base
	tag string
}

struct App {}

fn (mut app App) bump(mut b Base) {
	b.n += 2
}

fn dispatch[A, X](mut app A, mut ctx X) {
	$for method in A.methods {
		app.$method(mut ctx)
	}
}

// A reflected call passes the embedded struct of its first argument when the
// method expects that struct (as veb does with a custom Context that embeds
// veb.Context), so the checker must accept the embedding type there.
fn test_reflected_call_passes_embedded_struct_to_mut_param() {
	mut app := App{}
	mut ctx := Ctx{
		tag: 'x'
	}
	dispatch(mut app, mut ctx)
	assert ctx.n == 2
	assert ctx.tag == 'x'
}
