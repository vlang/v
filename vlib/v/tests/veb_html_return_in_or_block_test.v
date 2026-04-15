module main

import veb

struct Context {
	veb.Context
}

struct App {
mut:
	continued bool
}

struct Order {}

fn get_order(valid bool) !Order {
	if valid {
		return Order{}
	}
	return error('missing order')
}

fn (mut app App) index(mut ctx Context) veb.Result {
	record := get_order(false) or {
		ctx.res.set_status(.not_found)
		return $veb.html('templates/veb_html_return_in_or_block_test.html')
	}
	_ = record
	app.continued = true
	return ctx.text('continued')
}

fn test_veb_html_return_in_or_block_does_not_continue() {
	mut app := App{}
	mut ctx := Context{}
	_ = app.index(mut ctx)
	assert !app.continued
	assert ctx.res.status_code == 404
	assert ctx.res.body.trim_space() == 'Order not found!'
}
