module veb

import net.http
import os

struct ActivityContext {
	Context
}

@[heap]
struct ActivityApp {
	Middleware[ActivityContext]
	StaticHandler
	Controller
mut:
	begins int
	ends   int
	active int
}

fn (mut app ActivityApp) before_handle_request(_req http.Request) {
	assert app.active == 0
	app.active++
	app.begins++
}

fn (mut app ActivityApp) after_handle_request(_req http.Request) {
	assert app.active == 1
	app.active--
	app.ends++
}

@['/ok'; get]
fn (mut app ActivityApp) ok(mut ctx ActivityContext) Result {
	assert app.active == 1
	return ctx.text('ok')
}

@['/panic'; get]
fn (mut app ActivityApp) fail(mut ctx ActivityContext) Result {
	ctx.text('partial')
	panic('activity hook recovery fixture')
}

struct ActivityController {}

fn (app &ActivityController) index(mut ctx ActivityContext) Result {
	return ctx.text('controller')
}

fn activity_request(mut app ActivityApp, req http.Request, ssl bool) ?&Context {
	routes := generate_routes[ActivityApp, ActivityContext](app) or { panic(err) }
	controllers := check_duplicate_routes_in_controllers[ActivityApp](app, routes) or { panic(err) }
	if ssl {
		return handle_ssl_request[ActivityApp, ActivityContext](req, &SslRequestParams{ global_app: app, routes: &routes, controllers_sorted: controllers })
	}
	return handle_request_and_route[ActivityApp, ActivityContext](mut app, req, 0, RequestParams{ routes: &routes, controllers_sorted: controllers })
}

fn test_activity_hooks_balance_native_static_404_405_and_refusing_middleware() {
	for ssl in [false, true] {
		mut app := &ActivityApp{}
		mut controller := &ActivityController{}
		app.register_controller[ActivityController, ActivityContext]('/controller', mut controller) or { panic(err) }
		app.use(
			handler: fn (mut ctx ActivityContext) bool {
				if ctx.req.url == '/refused' {
					ctx.text('refused')
					return false
				}
				return true
			}
		)
		root := os.join_path(os.temp_dir(), 'veb-activity-${os.getpid()}-${ssl}')
		os.mkdir_all(root) or { panic(err) }
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(root, 'file.txt'), 'static') or { panic(err) }
		app.mount_static_folder_at(root, '/assets') or { panic(err) }
		statuses := [200, 404, 405, 200, 200, 500, 200]
		for i, path in ['/ok', '/missing', '/ok', '/refused', '/assets/file.txt', '/panic',
			'/controller'] {
			ctx := activity_request(mut app, http.Request{ method: if i == 2 { .post } else { .get }, url: path }, ssl) or { panic('no context for ${path}') }
			assert ctx.res.status_code == statuses[i], '${path}: ${ctx.res.status_code}'
			assert app.active == 0
			assert app.begins == i + 1
			assert app.ends == i + 1
		}
	}
}

fn test_activity_hooks_balance_when_form_parsing_fails_before_routing() {
	for ssl in [false, true] {
		mut app := &ActivityApp{}
		ctx := activity_request(mut app, http.Request{
			method: .post
			url:    '/ok'
			header: http.new_header_from_map({
				http.CommonHeader.content_type: 'application/json'
			})
			data:   '{'
		}, ssl) or {
			assert ssl
			assert app.begins == 1 && app.ends == 1 && app.active == 0
			continue
		}
		assert !ssl
		assert ctx.res.status_code == 400
		assert app.begins == 1 && app.ends == 1 && app.active == 0
	}
}
