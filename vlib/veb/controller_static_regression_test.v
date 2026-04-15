module veb

import net
import net.http
import net.urllib
import os

struct RegressionContext {
	Context
}

struct AppStaticRegressionApp {
	Controller
	StaticHandler
}

struct AppStaticRootController {}

fn (app &AppStaticRootController) index(mut ctx RegressionContext) Result {
	return ctx.text('from app static root controller')
}

struct ControllerStaticRootBase {
	StaticHandler
}

struct ControllerStaticRootController {
	ControllerStaticRootBase
}

fn (app &ControllerStaticRootController) index(mut ctx RegressionContext) Result {
	return ctx.text('from controller static root controller')
}

fn test_app_static_is_served_before_root_controller() {
	root_file := regression_testdata_path('root.txt')
	sub_dir := regression_testdata_path('sub_folder')
	sub_file := os.join_path(sub_dir, 'sub.txt')

	mut root_controller := AppStaticRootController{}
	mut app := AppStaticRegressionApp{}
	app.serve_static('/root.txt', root_file) or { panic(err) }
	app.handle_static(sub_dir, true) or { panic(err) }
	app.register_controller[AppStaticRootController, RegressionContext]('/', mut root_controller) or {
		panic(err)
	}

	completed_root := regression_handle_app_request(mut app, regression_request('/root.txt'))
	assert completed_root.return_type == .file
	assert completed_root.return_file == root_file

	completed_sub := regression_handle_app_request(mut app, regression_request('/sub.txt'))
	assert completed_sub.return_type == .file
	assert completed_sub.return_file == sub_file
}

fn test_controller_static_keeps_root_relative_paths() {
	root_file := regression_testdata_path('root.txt')
	sub_dir := regression_testdata_path('sub_folder')
	sub_file := os.join_path(sub_dir, 'sub.txt')

	mut controller_app := ControllerStaticRootController{}
	controller_app.serve_static('/root.txt', root_file) or { panic(err) }
	controller_app.handle_static(sub_dir, true) or { panic(err) }

	ctrl_path := controller[ControllerStaticRootController, RegressionContext]('/', mut
		controller_app) or { panic(err) }

	completed_root := regression_handle_controller_request(ctrl_path, '/root.txt')
	assert completed_root.return_type == .file
	assert completed_root.return_file == root_file

	completed_sub := regression_handle_controller_request(ctrl_path, '/sub.txt')
	assert completed_sub.return_type == .file
	assert completed_sub.return_file == sub_file
}

fn regression_request(url string) http.Request {
	return http.Request{
		method: .get
		url:    url
		header: http.new_custom_header_from_map({
			'Host': '127.0.0.1'
		}) or { panic(err) }
	}
}

fn regression_testdata_path(rel string) string {
	return os.join_path(os.dir(@FILE), 'tests', 'testdata', rel)
}

fn regression_handle_controller_request(ctrl_path &ControllerPath, path string) &Context {
	req := regression_request(path)
	mut url := urllib.parse_request_uri(path) or { panic(err) }
	ctx := &Context{
		req: req
	}
	return ctrl_path.handler(ctx, mut url, '')
}

fn regression_handle_app_request(mut app AppStaticRegressionApp, req http.Request) &Context {
	routes := generate_routes[AppStaticRegressionApp, RegressionContext](app) or { panic(err) }
	controllers_sorted := check_duplicate_routes_in_controllers[AppStaticRegressionApp](app, routes) or {
		panic(err)
	}

	$if new_veb ? {
		params := RequestParams{
			global_app:                unsafe { voidptr(&app) }
			controllers_sorted:        controllers_sorted
			routes:                    &routes
			benchmark_page_generation: false
		}
		return handle_request_and_route[AppStaticRegressionApp, RegressionContext](mut app, req, 0,
			params)
	} $else {
		params := &RequestParams{
			global_app:         unsafe { voidptr(&app) }
			controllers:        controllers_sorted
			routes:             &routes
			timeout_in_seconds: 2
		}
		mut conn := net.TcpConn{}
		return handle_request[AppStaticRegressionApp, RegressionContext](mut conn, req, params) or {
			panic(err)
		}
	}
}
