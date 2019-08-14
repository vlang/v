import vweb

const (
	Port = 8082
)

struct App {
	pub mut:
		vweb vweb.Context
}

fn main() {
	vweb.run<App>(Port)
}

pub fn (app mut App) init() {
	app.vweb.handle_static('.')
}

/*
fn (app mut App) test_headers() {
	mut ctx := app.vweb
	ctx.set_header('test', 'header')
	println(ctx.headers['test'])
	assert ctx.get_header('test') == 'header'
}

fn (app mut App) test_set_cookies() {
	mut ctx := app.vweb
	ctx.set_cookie('cookie', 'test')
	assert ctx.headers['Set-Cookie'].find_between(' cookie=', ';') == 'test'
}

fn (app mut App) test_cookies() {
	mut ctx := app.vweb
	ctx.set_cookie('cookie', 't1')
	ctx.headers['Cookie'] = ctx.headers['Set-Cookie'].clone()

	println(ctx.headers['Set-Cookie'])
	println(ctx.headers['Cookie'])
	test := ctx.get_cookie('cookie') or {
		return
	}
	println(test)

	assert ctx.get_cookie('cookie') == 't1'
}
*/