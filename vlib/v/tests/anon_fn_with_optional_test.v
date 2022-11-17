struct Response {}

pub struct Route {
	handler     fn (mut App) Response
	middlewares []fn (mut app App) ?Response
}

struct App {
	routes []Route
}

pub fn check_auth(mut app App) ?Response {
	return none
}

fn test_anon_fn_with_optional() {
	app := App{
		routes: [
			Route{
				middlewares: [
					check_auth,
				]
			},
		]
	}
	println(app)
	app_str := '${app}'
	assert app_str.contains('handler: fn (mut App) Response')
	assert app_str.contains('middlewares: [fn (mut App) ?Response]')
}
