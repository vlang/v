module veb

import strings

// openapi_json renders the app's route table as an OpenAPI 3.0 document: one
// path entry per route, one operation per method, with path parameters
// listed. It reads the same `generate_routes` output the dispatcher reads,
// so the document cannot drift from the code — it is the code, printed.
//
// Parameters are typed `string`: the route table records names, not declared
// kinds. Typed binding (`ctx.bind[T]`) will enrich this when it lands; until
// then a skeleton clients can generate against beats no document at all.
//
// ```v
// spec := veb.openapi_json[App, Context](app, 'Shop API', '1.0.0')!
// os.write_file('openapi.json', spec)!
// ```
pub fn openapi_json[A, X](app &A, title string, version string) !string {
	routes := generate_routes[A, X](app)!
	mut sb := strings.new_builder(1024)
	sb.write_string('{"openapi":"3.0.0","info":{"title":"')
	sb.write_string(openapi_escape(title))
	sb.write_string('","version":"')
	sb.write_string(openapi_escape(version))
	sb.write_string('"},"paths":{')
	mut first_path := true
	for _, route in routes {
		if !first_path {
			sb.write_string(',')
		}
		first_path = false
		write_openapi_path(mut sb, route)
	}
	sb.write_string('}}')
	return sb.str()
}

// write_openapi_path renders one route: its path with `:name` segments as
// `{name}`, one operation per HTTP method, path parameters listed.
fn write_openapi_path(mut sb strings.Builder, route Route) {
	openapi_path := openapi_path_of(route.path)
	sb.write_string('"')
	sb.write_string(openapi_escape(openapi_path))
	sb.write_string('":{')
	mut first_method := true
	for method in route.methods {
		if !first_method {
			sb.write_string(',')
		}
		first_method = false
		sb.write_string('"')
		sb.write_string(method.str().to_lower())
		sb.write_string('":{"parameters":[')
		write_openapi_parameters(mut sb, route.path)
		sb.write_string('],"responses":{"200":{"description":"OK"}}}}')
	}
	sb.write_string('}')
}

// openapi_path_of converts a veb route path to an OpenAPI path template:
// `:id` becomes `{id}`, and a trailing `:path...` becomes `{path}`.
fn openapi_path_of(path string) string {
	words := path.split('/')
	mut out := []string{cap: words.len}
	for word in words {
		if word.starts_with(':') {
			name := word.trim_left(':').trim_right('.')
			out << '{' + name + '}'
		} else {
			out << word
		}
	}
	return out.join('/')
}

// write_openapi_parameters lists the `:name` segments of a route path as
// OpenAPI path parameters, in order.
fn write_openapi_parameters(mut sb strings.Builder, path string) {
	mut first := true
	for word in path.split('/') {
		if !word.starts_with(':') {
			continue
		}
		name := word.trim_left(':').trim_right('.')
		if !first {
			sb.write_string(',')
		}
		first = false
		sb.write_string('{"name":"')
		sb.write_string(openapi_escape(name))
		sb.write_string('","in":"path","required":true,"schema":{"type":"string"}}')
	}
}

// openapi_escape renders a string safe for a JSON double-quoted value.
fn openapi_escape(s string) string {
	mut sb := strings.new_builder(s.len)
	for c in s.bytes() {
		match c {
			`"` {
				sb.write_string('\\"')
			}
			`\\` {
				sb.write_string('\\\\')
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\r` {
				sb.write_string('\\r')
			}
			`\t` {
				sb.write_string('\\t')
			}
			else {
				if c < 0x20 {
					sb.write_string('\\u00')
					sb.write_string(c.hex())
				} else {
					sb.write_string(c.ascii_str())
				}
			}
		}
	}
	return sb.str()
}
