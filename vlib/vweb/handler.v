module vweb

import net.http
import net.urllib
import time
import os

struct Handler<T> {
	global_app T
}

fn (h Handler<T>) handle(req http.Request) http.Response {
	// Create a new app object for each connection, copy global data like db connections
	mut request_app := &T{}
	$if T is DbInterface {
		request_app.db = global_app.db
	} $else {
		// println('vweb no db')
	}
	$for field in T.fields {
		if field.is_shared {
			request_app.$(field.name) = h.global_app.$(field.name)
		}
	}
	request_app.Context = h.global_app.Context // copy the context ref that contains static files map etc
	// request_app.Context = Context{
	// conn: 0
	//}
	handle_request<T>(mut request_app, req)
	if !request_app.done {
		eprintln('Handled request, but Context.done is not set!')
		return http_500
	}
	return request_app.resp
}

[manualfree]
fn handle_request<T>(mut app T, req http.Request) {
	page_gen_start := time.ticks()
	app.Context = Context{
		req: req
		form: map[string]string{}
		static_files: app.static_files
		static_mime_types: app.static_mime_types
		page_gen_start: page_gen_start
	}
	if req.method in methods_with_form {
		ct := req.header.get(.content_type) or { '' }.split(';').map(it.trim_left(' \t'))
		if 'multipart/form-data' in ct {
			boundary := ct.filter(it.starts_with('boundary='))
			if boundary.len != 1 {
				app.done = true
				app.resp = http_400
				return
			}
			form, files := parse_multipart_form(req.data, boundary[0][9..])
			for k, v in form {
				app.form[k] = v
			}
			for k, v in files {
				app.files[k] = v
			}
		} else {
			form := parse_form(req.data)
			for k, v in form {
				app.form[k] = v
			}
		}
	}
	// Serve a static file if it is one
	// TODO: get the real path
	url := urllib.parse(app.req.url) or {
		eprintln('error parsing path: $err')
		app.done = true
		app.resp = http_400
		return
	}
	if serve_if_static<T>(mut app, url) {
		// successfully served a static file
		return
	}

	app.before_request()
	// Call the right action
	$if debug {
		println('route matching...')
	}
	url_words := url.path.split('/').filter(it != '')
	// copy query args to app.query
	for k, v in url.query().data {
		app.query[k] = v.data[0]
	}

	$for method in T.methods {
		$if method.return_type is Result {
			mut method_args := []string{}
			// TODO: move to server start
			http_methods, route_path := parse_attrs(method.name, method.attrs) or {
				eprintln('error parsing method attributes: $err')
				app.done = true
				app.resp = http_500
				return
			}

			// Used for route matching
			route_words := route_path.split('/').filter(it != '')

			// Skip if the HTTP request method does not match the attributes
			if app.req.method in http_methods {
				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route_path.contains('/:') && url_words == route_words {
					// We found a match
					app.$method()
					return
				}

				if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
					app.$method()
					return
				}

				if params := route_matches(url_words, route_words) {
					method_args = params.clone()
					if method_args.len != method.args.len {
						eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($method_args.len)')
					}
					app.$method(method_args)
					return
				}
			}
		}
	}
	// site not found
	app.done = true
	app.resp = http_404
}

// check if request is for a static file and serves it
// returns true if we served a static file, false otherwise
[manualfree]
fn serve_if_static<T>(mut app T, url urllib.URL) bool {
	// TODO: handle url parameters properly - for now, ignore them
	static_file := app.static_files[url.path]
	mime_type := app.static_mime_types[url.path]
	if static_file == '' || mime_type == '' {
		return false
	}
	data := os.read_file(static_file) or {
		app.done = true
		app.resp = http_404
		return true
	}
	app.set_response(mime_type, data)
	unsafe { data.free() }
	return true
}
