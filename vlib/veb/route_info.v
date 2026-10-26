module veb

import json2
import net.http

// RouteInfo describes a dispatched route, including its mounted controller path.
pub struct RouteInfo {
pub:
	name               string
	controller         string
	path               string
	methods            []http.Method
	host               string
	parameter_names    []string
	parameter_types    []string
	parameter_schemas  []map[string]json2.Any
	parameter_required []bool
}

// route_info returns root and recursively registered controller routes from the dispatcher metadata.
pub fn route_info[A, X](app &A) ![]RouteInfo {
	return flatten_route_info[A](app, generate_routes[A, X](app)!)
}

fn flatten_route_info[A](app &A, routes map[string]Route) []RouteInfo {
	mut result := []RouteInfo{}
	mut parameter_schemas := map[string][]map[string]json2.Any{}
	$for method in A.methods {
		$if method.return_type is Result || method.return_type is !Result {
			mut schemas := []map[string]json2.Any{}
			$for param in method.params {
				// Classify reflected parameter types directly; param.typ is not a generic value.
				$if param.typ is u64 {
					schemas << openapi_integer_schema(u64(0))
				} $else $if param.typ is uint {
					schemas << openapi_integer_schema(uint(0))
				} $else $if param.typ is u32 {
					schemas << openapi_integer_schema(u32(0))
				} $else $if param.typ is i64 {
					schemas << openapi_integer_schema(i64(0))
				} $else $if param.typ is int {
					schemas << openapi_integer_schema(int(0))
				} $else $if param.typ is $int {
					schemas << openapi_integer_schema(int(0))
				} $else $if param.typ is bool {
					schemas << map[string]json2.Any{
						'type': json2.Any('boolean')
					}
				} $else {
					schemas << map[string]json2.Any{
						'type': json2.Any('string')
					}
				}
			}
			if schemas.len > 0 { schemas = schemas[1..] }
			parameter_schemas[method.name] = schemas
		}
	}
	for name, route in routes {
		mut kinds := []string{}
		mut required := []bool{}
		schemas := parameter_schemas[name]
		for schema in schemas { kinds << (schema['type'] or { json2.Any('string') }).str() }
		for kind in route.parameter_kinds {
			// Preserve the dispatcher's missing-value policy independently of schema widths.
			required << kind in [.integer, .boolean]
		}
		info := RouteInfo{
			name:               name
			controller:         typeof(app).name.trim_left('&')
			path:               openapi_join_path('', route.path)
			methods:            route.methods
			host:               route.host
			parameter_names:    route.parameter_names
			parameter_types:    kinds
			parameter_schemas:  schemas
			parameter_required: required
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

fn (route &RouteInfo) parameter_schema(index int) OpenApiSchema {
	if index < route.parameter_schemas.len { return route.parameter_schemas[index] }
	return {
		'type': json2.Any('string')
	}
}

fn (route &RouteInfo) requires_parameter(index int) bool {
	return index < route.parameter_required.len && route.parameter_required[index]
}

fn openapi_join_path(prefix string, path string) string {
	return '/' + (prefix + '/' + path).split('/').filter(it != '').join('/')
}
