module veb

import json2
import net.http

type OpenApiHandlerCallback = fn (mut OpenApiDocument, string, http.Method, string) !

// RouteInfo describes a dispatched route, including its mounted controller path.
pub struct RouteInfo {
	openapi_metadata_name             string
	openapi_metadata                  OpenApiHandlerCallback = unsafe { nil }
	openapi_controller_metadata_name  string
	openapi_controller_metadata       OpenApiHandlerCallback = unsafe { nil }
	openapi_controller_metadata_count int
	openapi_summary                   string
	openapi_operation_id              string
	openapi_tags                      []OpenApiTagConfig
	openapi_tag_groups                []OpenApiTagGroup
	openapi_tags_method_count         int
pub:
	name               string
	controller         string
	path               string
	methods            []http.Method
	host               string
	description        string
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
	mut summaries := map[string]string{}
	mut operation_ids := map[string]string{}
	$for method in A.methods {
		$for attr in method.attributes {
			$if attr is OpenApiSummary {
				summaries[method.name] = attr.value
			}
			$if attr is OpenApiOperationId {
				operation_ids[method.name] = attr.value
			}
		}
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
	mut metadata_callbacks := map[string]OpenApiHandlerCallback{}
	mut metadata_names := map[string]string{}
	mut controller_metadata_names := []string{}
	$for method in A.methods {
		$for attr in method.attributes {
			$if attr is OpenApi {
				metadata_names[method.name] = attr.handler
			}
			$if attr is OpenApiMetadata {
				controller_metadata_names << method.name
			}
		}
		$if method.args.len == 1 {
			$for param in method.params {
				$if param.typ is &OpenApiHandler {
					metadata_callbacks[method.name] = fn [method, app] [A](mut doc OpenApiDocument, path string, verb http.Method, handler_name string) ! {
						mut handler := OpenApiHandler{
							doc:     &doc
							path:    path
							method:  verb
							handler: handler_name
						}
						app.$method(mut handler)!
					}
				}
			}
		}
	}
	mut controller_tags := []OpenApiTagConfig{}
	$for attr in A.attributes {
		$if attr is OpenApiTag {
			controller_tags << OpenApiTagConfig{
				name:    attr.name
				summary: attr.summary
				parent:  attr.parent
			}
		}
	}
	mut controller_tags_methods := []string{}
	$for method in A.methods {
		$for attr in method.attributes {
			$if attr is OpenApiTags {
				controller_tags_methods << method.name
			}
		}
	}
	controller_tags_method := if controller_tags_methods.len > 0 {
		controller_tags_methods[0]
	} else {
		''
	}
	if controller_tags_method != '' {
		$for method in A.methods {
			if method.name == controller_tags_method {
				controller_tags << app.$method()
			}
		}
	}
	mut controller_tag_groups := []OpenApiTagGroup{}
	$if A is ControllerInterface {
		controller_tag_groups = app.openapi_tag_groups.clone()
	}
	controller_metadata_name := if controller_metadata_names.len > 0 {
		controller_metadata_names[0]
	} else {
		''
	}
	controller_metadata := if controller_metadata_names.len == 1 {
		metadata_callbacks[controller_metadata_name] or {
			OpenApiHandlerCallback(unsafe { nil })
		}
	} else {
		OpenApiHandlerCallback(unsafe { nil })
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
		metadata_name := metadata_names[name]
		info := RouteInfo{
			name:                              name
			controller:                        typeof(app).name.trim_left('&')
			path:                              openapi_join_path('', route.path)
			methods:                           route.methods
			host:                              route.host
			description:                       route_description(route.attrs)
			parameter_names:                   route.parameter_names
			parameter_types:                   kinds
			parameter_schemas:                 schemas
			parameter_required:                required
			openapi_metadata_name:             metadata_name
			openapi_metadata:                  metadata_callbacks[metadata_name] or { OpenApiHandlerCallback(unsafe { nil }) }
			openapi_controller_metadata_name:  controller_metadata_name
			openapi_controller_metadata:       controller_metadata
			openapi_controller_metadata_count: controller_metadata_names.len
			openapi_summary:                   summaries[name]
			openapi_operation_id:              operation_ids[name]
			openapi_tags:                      controller_tags.clone()
			openapi_tag_groups:                controller_tag_groups.clone()
			openapi_tags_method_count:         controller_tags_methods.len
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
				mut tag_groups := controller_tag_groups.clone()
				for group in route.openapi_tag_groups {
					if group !in tag_groups {
						tag_groups << group
					}
				}
				result << RouteInfo{
					...route
					path:               openapi_join_path(child.path, route.path)
					host:               if route.host != '' { route.host } else { child.host }
					openapi_tag_groups: tag_groups
				}
			}
		}
	}
	result.sort(a.path < b.path)
	return result
}

fn route_description(attrs []string) string {
	for attr in attrs {
		if attr.starts_with('description:') {
			value := attr.all_after(':').trim_space()
			// Reflection decodes ordinary strings; raw strings retain their prefix.
			quote_index := if value.len > 2 && value[0] == `r` { 1 } else { 0 }
			if value.len > quote_index + 1 && value[quote_index] == value[value.len - 1]
				&& value[quote_index] in [`'`, `"`] {
				return value[quote_index + 1..value.len - 1]
			}
			return value
		}
	}
	return ''
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
