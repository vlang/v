module veb

import net.http

// RouteInfo describes a dispatched route, including its mounted controller path.
pub struct RouteInfo {
pub:
	name            string
	controller      string
	path            string
	methods         []http.Method
	host            string
	parameter_names []string
	parameter_types []string
}

// route_info returns root and recursively registered controller routes from the dispatcher metadata.
pub fn route_info[A, X](app &A) ![]RouteInfo {
	return flatten_route_info[A](app, generate_routes[A, X](app)!)
}

fn flatten_route_info[A](app &A, routes map[string]Route) []RouteInfo {
	mut result := []RouteInfo{}
	for name, route in routes {
		mut kinds := []string{}
		for kind in route.parameter_kinds {
			kinds << match kind {
				.integer { 'integer' }
				.boolean { 'boolean' }
				else { 'string' }
			}
		}
		info := RouteInfo{
			name:            name
			controller:      typeof(app).name.trim_left('&')
			path:            openapi_join_path('', route.path)
			methods:         route.methods
			host:            route.host
			parameter_names: route.parameter_names
			parameter_types: kinds
		}
		result << info
		if name == 'index' && route.path == '/index' {
			result << RouteInfo{ ...info, path: '/' }
		}
	}
	$if A is ControllerInterface {
		for child in app.controllers {
			for route in child.documented_routes {
				if child.host != '' && route.host != '' && child.host != route.host {
					continue
				}
				result << RouteInfo{
					...route
					path: openapi_join_path(child.path, route.path)
					host: if route.host != '' { route.host } else { child.host }
				}
			}
		}
	}
	result.sort(a.path < b.path)
	return result
}

fn openapi_join_path(prefix string, path string) string {
	return '/' + (prefix + '/' + path).split('/').filter(it != '').join('/')
}
