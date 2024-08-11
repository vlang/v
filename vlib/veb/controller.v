module veb

import net.urllib

type ControllerHandler = fn (ctx &Context, mut url urllib.URL, host string) &Context

pub struct ControllerPath {
pub:
	path    string
	handler ControllerHandler = unsafe { nil }
pub mut:
	host string
}

interface ControllerInterface {
	controllers []&ControllerPath
}

pub struct Controller {
pub mut:
	controllers []&ControllerPath
}

// register_controller adds a new Controller to your app
pub fn (mut c Controller) register_controller[A, X](path string, mut global_app A) ! {
	c.controllers << controller[A, X](path, mut global_app)!
}

// controller generates a new Controller for the main app
pub fn controller[A, X](path string, mut global_app A) !&ControllerPath {
	routes := generate_routes[A, X](global_app) or { panic(err.msg()) }
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!

	// generate struct with closure so the generic type is encapsulated in the closure
	// no need to type `ControllerHandler` as generic since it's not needed for closures
	return &ControllerPath{
		path:    path
		handler: fn [mut global_app, path, routes, controllers_sorted] [A, X](ctx &Context, mut url urllib.URL, host string) &Context {
			// transform the url
			url.path = url.path.all_after_first(path)

			// match controller paths
			$if A is ControllerInterface {
				if completed_context := handle_controllers[X](controllers_sorted, ctx, mut
					url, host)
				{
					return completed_context
				}
			}

			// create a new user context and pass the veb's context
			mut user_context := X{}
			user_context.Context = ctx

			handle_route[A, X](mut global_app, mut user_context, url, host, &routes)
			// we need to explicitly tell the V compiler to return a reference
			return &user_context.Context
		}
	}
}

// register_controller adds a new Controller to your app
pub fn (mut c Controller) register_host_controller[A, X](host string, path string, mut global_app A) ! {
	c.controllers << controller_host[A, X](host, path, mut global_app)!
}

// controller_host generates a controller which only handles incoming requests from the `host` domain
pub fn controller_host[A, X](host string, path string, mut global_app A) &ControllerPath {
	mut ctrl := controller[A, X](path, mut global_app)
	ctrl.host = host
	return ctrl
}

fn check_duplicate_routes_in_controllers[T](global_app &T, routes map[string]Route) ![]&ControllerPath {
	mut controllers_sorted := []&ControllerPath{}
	$if T is ControllerInterface {
		mut paths := []string{}
		controllers_sorted = global_app.controllers.clone()
		controllers_sorted.sort(a.path.len > b.path.len)
		for controller in controllers_sorted {
			if controller.host == '' {
				if controller.path in paths {
					return error('conflicting paths: duplicate controller handling the route "${controller.path}"')
				}
				paths << controller.path
			}
		}
		for method_name, route in routes {
			for controller_path in paths {
				if route.path.starts_with(controller_path) {
					return error('conflicting paths: method "${method_name}" with route "${route.path}" should be handled by the Controller of path "${controller_path}"')
				}
			}
		}
	}
	return controllers_sorted
}

fn handle_controllers[X](controllers []&ControllerPath, ctx &Context, mut url urllib.URL, host string) ?&Context {
	for controller in controllers {
		// skip controller if the hosts don't match
		if controller.host != '' && host != controller.host {
			continue
		}
		if url.path.len >= controller.path.len && url.path.starts_with(controller.path) {
			// pass route handling to the controller
			return controller.handler(ctx, mut url, host)
		}
	}

	return none
}
