module veb

import net.urllib
import net.http
import json2

// Parsing function attributes for methods and path.
fn parse_attrs(name string, attrs []string) !([]http.Method, string, string) {
	if attrs.len == 0 {
		return [http.Method.get], '/${name}', ''
	}

	mut x := attrs.clone()
	mut methods := []http.Method{}
	mut path := ''
	mut host := ''

	for i := 0; i < x.len; {
		attr := x[i]
		attru := attr.to_upper()
		m := http.method_from_str(attru)
		if attru == 'UNSAFE' {
			x.delete(i)
			continue
		}
		if attru == 'GET' || m != .get {
			methods << m
			x.delete(i)
			continue
		}
		if attr.starts_with('/') {
			if path != '' {
				return http.MultiplePathAttributesError{}
			}
			path = attr
			x.delete(i)
			continue
		}
		if attr.starts_with('host:') {
			host = attr.all_after('host:').trim_space().trim('\'"')
			x.delete(i)
			continue
		}
		i++
	}
	if x.len > 0 {
		return http.UnexpectedExtraAttributeError{
			attributes: x
		}
	}
	if methods.len == 0 {
		methods = [http.Method.get]
	}
	if path == '' {
		path = '/${name}'
	}
	// Make host lowercase for case-insensitive comparisons
	return methods, path, host.to_lower()
}

fn parse_query_from_url(url urllib.URL) map[string]string {
	mut query := map[string]string{}
	for qvalue in url.query().data {
		query[qvalue.key] = qvalue.value
	}
	return query
}

const boundary_start = 'boundary='

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

// TODO: fix windows files? (CLRF) issues, maybe it is in the `net` module
fn parse_form_from_request(request http.Request) !(map[string]string, map[string][]http.FileData) {
	if request.method !in [http.Method.post, .put, .patch] {
		return map[string]string{}, map[string][]http.FileData{}
	}
	ct := request.header.get(.content_type) or { '' }.split(';').map(it.trim_left(' \t'))
	if 'multipart/form-data' in ct {
		boundaries := ct.filter(it.starts_with(boundary_start))
		if boundaries.len != 1 {
			return error('detected more that one form-data boundary')
		}
		boundary := boundaries[0].all_after(boundary_start)
		if boundary.len > 0 && boundary[0] == `"` {
			// quotes are send by our http.post_multipart_form/2:
			return http.parse_multipart_form(request.data, boundary.trim('"'))
		}
		// Firefox and other browsers, do not use quotes around the boundary:
		return http.parse_multipart_form(request.data, boundary)
	}
	if json_media_type in ct {
		return parse_json_form(request.data)
	}
	return http.parse_form(request.data), map[string][]http.FileData{}
}

// json_media_type is the content type a JSON request body is read as.
const json_media_type = 'application/json'

// parse_json_form decodes a JSON object body into the same flat form map a
// urlencoded body fills, so a handler reads `ctx.form['name']` whichever
// encoding the client used.
// A nested object or an array is skipped rather than flattened: a form field
// holds one scalar value, and handing a handler a mangled copy of the structure
// would hide the fact that it needs a typed decode instead. A body that is not
// a JSON object is an error, so it never looks like an empty form.
fn parse_json_form(body string) !(map[string]string, map[string][]http.FileData) {
	values := json2.decode[map[string]json2.Any](body) or {
		return error('the JSON body is not an object: ${err.msg()}')
	}
	mut form := map[string]string{}
	for key, value in values {
		if scalar := json_form_value(value) {
			form[key] = scalar
		}
	}
	return form, map[string][]http.FileData{}
}

// json_form_value renders one decoded JSON value as a form field value, or
// returns `none` for the shapes a flat map cannot represent.
fn json_form_value(value json2.Any) ?string {
	if value is map[string]json2.Any || value is []json2.Any || value is json2.Null {
		return none
	}
	return value.str()
}

// has_route_attributes checks if a method has attributes that indicate it should be a route handler
fn has_route_attributes(attrs []string) bool {
	if attrs.len == 0 {
		return false
	}
	for attr in attrs {
		attru := attr.to_upper()
		if attru in ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'HEAD', 'OPTIONS'] {
			return true
		}
		if attr.starts_with('/') {
			return true
		}
		if attr.starts_with('host:') {
			return true
		}
	}
	return false
}
