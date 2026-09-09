import veb

pub struct NestedEmbeddedContext {
	veb.Context
}

pub struct NestedEmbeddedApp {
	veb.Middleware[NestedEmbeddedContext]
	veb.Controller
	veb.StaticHandler
}

struct NestedEmbeddedAliasApp {
	NestedEmbeddedApp
}

struct NestedEmbeddedBase {
	veb.Middleware[NestedEmbeddedContext]
}

fn test_nested_embedded_controller_registration() {
	mut app := &NestedEmbeddedAliasApp{}
	app.routes_base()
	routes := veb.generate_routes[NestedEmbeddedAliasApp, NestedEmbeddedContext](app) or {
		panic(err)
	}
	assert app.controllers.len == 1
	assert app.controllers[0].path == '/base'
	assert 'index' in routes
}

fn exercise_nested_embedded_run_path() {
	mut app := &NestedEmbeddedAliasApp{}
	app.routes_base()
	veb.run_at[NestedEmbeddedAliasApp, NestedEmbeddedContext](mut app,
		port:                 0
		show_startup_message: false
		family:               .ip
		timeout_in_seconds:   1
	) or { panic(err) }
}

fn (mut app NestedEmbeddedAliasApp) routes_base() {
	mut base_app := &NestedEmbeddedBase{}
	app.register_controller[NestedEmbeddedBase, NestedEmbeddedContext]('/base', mut base_app) or {
		panic(err)
	}
}

@['/'; get]
fn (mut app NestedEmbeddedAliasApp) index(mut ctx NestedEmbeddedContext) veb.Result {
	return ctx.text('index success')
}

@['/get'; get]
fn (mut app NestedEmbeddedBase) base(mut ctx NestedEmbeddedContext) veb.Result {
	return ctx.text('base success')
}
