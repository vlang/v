// Reading the routes a veb web application registers.
//
// A route is a method decorated with an attribute naming an HTTP method:
//
//	@[get]
//	pub fn (mut app App) index(mut ctx Context) veb.Result { ... }
//
//	@[post; /submit]
//	pub fn (mut app App) submit(mut ctx Context) veb.Result { ... }
//
// The parser keeps the attribute list in a `.directive` node whose value names the
// node it belongs to (`@attributes:<node index>`), and the attribute text itself in
// the node payload. That link is what this file walks: it is how a route is tied
// to the handler it serves without re-parsing the source text.
module main

import v.astjson
import v.astquery
import v.flat

// http_methods are the attribute names that register a route.
const http_methods = ['get', 'post', 'put', 'delete', 'patch', 'head', 'options']

// Route is one registered web route.
pub struct Route {
pub mut:
	// method is the HTTP method, lowercase. veb registers `get` when the attribute
	// names no method, so an answer always has one.
	method string
	// path is the URL path, always starting with `/`.
	path string
	// handler is the qualified name of the method that serves the route.
	handler string
	// line and column are where the handler's name sits.
	line   int
	column int
}

// route_json renders one route.
fn route_json(route Route) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('method')
	w.string(route.method)
	w.key('path')
	w.string(route.path)
	w.key('handler')
	w.string(route.handler)
	w.key('line')
	w.number(route.line)
	w.key('column')
	w.number(route.column)
	w.end_object()
	return w.str()
}

// routes_json renders a list of routes as a JSON array.
fn routes_json(routes []Route) string {
	mut w := astjson.Writer{}
	w.begin_array()
	for route in routes {
		w.array_raw(route_json(route))
	}
	w.end_array()
	return w.str()
}

// veb_routes returns every route registered in `path`, in source order.
//
// The file is parsed once: the same AST answers both which functions exist and
// which attributes each one carries.
fn veb_routes(path string) []Route {
	a := astquery.parse(path)
	attributes := attributes_by_target(a)
	mut out := []Route{}
	for decl in astquery.declarations(a) {
		if decl.kind != .fn && decl.kind != .method {
			continue
		}
		index := fn_node_index(a, decl) or { continue }
		list := attributes[index] or { continue }
		if !list.any(is_http_method) {
			continue
		}
		out << route_from_attributes(list, decl)
	}
	return out
}

// fn_node_index returns the node index of the function declaration `decl`
// describes, matched on the name the AST gives the function.
fn fn_node_index(a &flat.FlatAst, decl astquery.Declaration) ?int {
	for index, node in a.nodes {
		if node.kind != .fn_decl && node.kind != .c_fn_decl {
			continue
		}
		if node.value == decl.name || node.value.all_after_last('.') == decl.name {
			return index
		}
	}
	return none
}

// is_http_method reports whether an attribute names an HTTP method.
fn is_http_method(entry string) bool {
	return entry.to_lower() in http_methods
}

// attributes_by_target maps a declaration's node index to its attribute text.
//
// Only the parser's own `@attributes:<index>` directives are considered, so an
// unrelated attribute cannot be mistaken for a route.
fn attributes_by_target(a &flat.FlatAst) map[int][]string {
	mut out := map[int][]string{}
	for node in a.nodes {
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		target := node.value['@attributes:'.len..].int()
		mut list := []string{}
		for entry in node.generic_params() {
			list << entry.trim('\'"')
		}
		out[target] = list
	}
	return out
}

// route_from_attributes turns one attribute list into a route.
//
// veb's own rules are followed: the first named method wins, `unsafe` and
// `key: value` entries configure rather than route, and a handler with no path
// attribute is served at `/<name>`.
fn route_from_attributes(attributes []string, decl astquery.Declaration) Route {
	mut route := Route{
		method:  ''
		path:    ''
		handler: decl.name
		line:    decl.line
		column:  decl.column
	}
	for entry in attributes {
		lower := entry.to_lower()
		if lower in http_methods {
			if route.method == '' {
				route.method = lower
			}
			continue
		}
		if lower == 'unsafe' || entry.contains(': ') {
			continue
		}
		if route.path == '' {
			route.path = if entry.starts_with('/') { entry } else { '/' + entry }
		}
	}
	if route.method == '' {
		route.method = 'get'
	}
	if route.path == '' {
		route.path = '/' + decl.name.all_after_last('.')
	}
	return route
}
