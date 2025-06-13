module veb

import compress.gzip
import net.http

pub type MiddlewareHandler[T] = fn (mut T) bool

// TODO: get rid of this `voidptr` interface check when generic embedded
// interfaces work properly, related: #19968
interface MiddlewareApp {
mut:
	global_handlers       []voidptr
	global_handlers_after []voidptr
	route_handlers        []RouteMiddleware
	route_handlers_after  []RouteMiddleware
}

struct RouteMiddleware {
	url_parts []string
	handler   voidptr
}

pub struct Middleware[T] {
mut:
	global_handlers       []voidptr
	global_handlers_after []voidptr
	route_handlers        []RouteMiddleware
	route_handlers_after  []RouteMiddleware
}

@[params]
pub struct MiddlewareOptions[T] {
pub:
	handler fn (mut ctx T) bool @[required]
	after   bool
}

// string representation of Middleware
pub fn (m &Middleware[T]) str() string {
	return 'veb.Middleware[${T.name}]{
        global_handlers: [${m.global_handlers.len}]
        global_handlers_after: [${m.global_handlers_after.len}]
        route_handlers: [${m.route_handlers.len}]
        route_handlers_after: [${m.route_handlers_after.len}]
    }'
}

// use registers a global middleware handler
pub fn (mut m Middleware[T]) use(options MiddlewareOptions[T]) {
	if options.after {
		m.global_handlers_after << voidptr(options.handler)
	} else {
		m.global_handlers << voidptr(options.handler)
	}
}

// route_use registers a middleware handler for a specific route(s)
pub fn (mut m Middleware[T]) route_use(route string, options MiddlewareOptions[T]) {
	middleware := RouteMiddleware{
		url_parts: route.split('/').filter(it != '')
		handler:   voidptr(options.handler)
	}

	if options.after {
		m.route_handlers_after << middleware
	} else {
		m.route_handlers << middleware
	}
}

fn (m &Middleware[T]) get_handlers_for_route(route_path string) []voidptr {
	mut fns := []voidptr{}
	route_parts := route_path.split('/').filter(it != '')

	for handler in m.route_handlers {
		if _ := route_matches(route_parts, handler.url_parts) {
			fns << handler.handler
		} else if handler.url_parts.len == 0 && route_path == '/index' {
			fns << handler.handler
		}
	}

	return fns
}

fn (m &Middleware[T]) get_handlers_for_route_after(route_path string) []voidptr {
	mut fns := []voidptr{}
	route_parts := route_path.split('/').filter(it != '')

	for handler in m.route_handlers_after {
		if _ := route_matches(route_parts, handler.url_parts) {
			fns << handler.handler
		} else if handler.url_parts.len == 0 && route_path == '/index' {
			fns << handler.handler
		}
	}

	return fns
}

fn (m &Middleware[T]) get_global_handlers() []voidptr {
	return m.global_handlers
}

fn (m &Middleware[T]) get_global_handlers_after() []voidptr {
	return m.global_handlers_after
}

fn validate_middleware[T](mut ctx T, raw_handlers []voidptr) bool {
	for handler in raw_handlers {
		func := MiddlewareHandler[T](handler)
		if func(mut ctx) == false {
			return false
		}
	}

	return true
}

// encode_gzip adds gzip encoding to the HTTP Response body.
// This middleware does not encode files, if you return `ctx.file()`.
// Register this middleware as last!
// Example: app.use(veb.encode_gzip[Context]())
pub fn encode_gzip[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [T](mut ctx T) bool {
			// TODO: compress file in streaming manner, or precompress them?
			if ctx.return_type == .file {
				return true
			}
			// first try compressions, because if it fails we can still send a response
			// before taking over the connection
			compressed := gzip.compress(ctx.res.body.bytes()) or {
				eprintln('[veb] error while compressing with gzip: ${err.msg()}')
				return true
			}
			// enables us to have full control over what response is send over the connection
			// and how.
			ctx.takeover_conn()

			// set HTTP headers for gzip
			ctx.res.header.add(.content_encoding, 'gzip')
			ctx.res.header.set(.vary, 'Accept-Encoding')
			ctx.res.header.set(.content_length, compressed.len.str())

			fast_send_resp_header(mut ctx.Context.conn, ctx.res) or {}
			ctx.Context.conn.write_ptr(&u8(compressed.data), compressed.len) or {}
			ctx.Context.conn.close() or {}

			return false
		}
	}
}

// decode_gzip decodes the body of a gzip'ed HTTP request.
// Register this middleware before you do anything with the request body!
// Example: app.use(veb.decode_gzip[Context]())
pub fn decode_gzip[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [T](mut ctx T) bool {
			if encoding := ctx.res.header.get(.content_encoding) {
				if encoding == 'gzip' {
					decompressed := gzip.decompress(ctx.req.body.bytes()) or {
						ctx.request_error('invalid gzip encoding')
						return false
					}
					ctx.req.body = decompressed.bytestr()
				}
			}
		}
	}
}

interface HasBeforeRequest {
	before_request()
}

pub const cors_safelisted_response_headers = [http.CommonHeader.cache_control, .content_language,
	.content_length, .content_type, .expires, .last_modified, .pragma].map(it.str()).join(',')

// CorsOptions is used to set CORS response headers.
// See https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS#the_http_response_headers
@[params]
pub struct CorsOptions {
pub:
	// from which origin(s) can cross-origin requests be made; `Access-Control-Allow-Origin`
	origins []string @[required]
	// indicate whether the server allows credentials, e.g. cookies, in cross-origin requests.
	// ;`Access-Control-Allow-Credentials`
	allow_credentials bool
	// allowed HTTP headers for a cross-origin request; `Access-Control-Allow-Headers`
	allowed_headers []string
	// allowed HTTP methods for a cross-origin request; `Access-Control-Allow-Methods`
	allowed_methods []http.Method
	// indicate if clients are able to access other headers than the "CORS-safelisted"
	// response headers; `Access-Control-Expose-Headers`
	expose_headers []string
	// how long the results of a preflight request can be cached, value is in seconds
	// ; `Access-Control-Max-Age`
	max_age ?int
}

// set_headers adds the CORS headers on the response
pub fn (options &CorsOptions) set_headers(mut ctx Context) {
	// A browser will reject a CORS request when the Access-Control-Allow-Origin header
	// is not present. By not setting the CORS headers when an invalid origin is supplied
	// we force the browser to reject the preflight and the actual request.
	origin := ctx.req.header.get(.origin) or { return }
	if options.origins != ['*'] && origin !in options.origins {
		return
	}

	ctx.set_header(.access_control_allow_origin, origin)
	ctx.set_header(.vary, 'Origin, Access-Control-Request-Headers')

	// dont' set the value of `Access-Control-Allow-Credentials` to 'false', but
	// omit the header if the value is `false`
	if options.allow_credentials {
		ctx.set_header(.access_control_allow_credentials, 'true')
	}

	if options.allowed_headers.len > 0 {
		ctx.set_header(.access_control_allow_headers, options.allowed_headers.join(','))
	} else if _ := ctx.req.header.get(.access_control_request_headers) {
		// a server must respond with `Access-Control-Allow-Headers` if
		// `Access-Control-Request-Headers` is present in a preflight request
		ctx.set_header(.access_control_allow_headers, cors_safelisted_response_headers)
	}

	if options.allowed_methods.len > 0 {
		method_str := options.allowed_methods.str().trim('[]')
		ctx.set_header(.access_control_allow_methods, method_str)
	}

	if options.expose_headers.len > 0 {
		ctx.set_header(.access_control_expose_headers, options.expose_headers.join(','))
	}

	if max_age := options.max_age {
		ctx.set_header(.access_control_max_age, max_age.str())
	}
}

// validate_request checks if a cross-origin request is made and verifies the CORS
// headers. If a cross-origin request is invalid this method will send a response
// using `ctx`.
pub fn (options &CorsOptions) validate_request(mut ctx Context) bool {
	origin := ctx.req.header.get(.origin) or { return true }
	if options.origins != ['*'] && origin !in options.origins {
		ctx.res.set_status(.forbidden)
		ctx.text('invalid CORS origin')

		$if veb_trace_cors ? {
			eprintln('[veb]: rejected CORS request from "${origin}". Reason: invalid origin')
		}
		return false
	}

	ctx.set_header(.access_control_allow_origin, origin)
	ctx.set_header(.vary, 'Origin, Access-Control-Request-Headers')

	if options.allow_credentials {
		ctx.set_header(.access_control_allow_credentials, 'true')
	}

	// validate request method
	if ctx.req.method !in options.allowed_methods {
		ctx.res.set_status(.method_not_allowed)
		ctx.text('${ctx.req.method} requests are not allowed')

		$if veb_trace_cors ? {
			eprintln('[veb]: rejected CORS request from "${origin}". Reason: invalid request method: ${ctx.req.method}')
		}
		return false
	}

	if options.allowed_headers.len > 0 && options.allowed_headers != ['*'] {
		// validate request headers
		for header in ctx.req.header.keys() {
			if header !in options.allowed_headers {
				ctx.res.set_status(.forbidden)
				ctx.text('invalid Header "${header}"')

				$if veb_trace_cors ? {
					eprintln('[veb]: rejected CORS request from "${origin}". Reason: invalid header "${header}"')
				}
				return false
			}
		}
	}

	$if veb_trace_cors ? {
		eprintln('[veb]: received CORS request from "${origin}": HTTP ${ctx.req.method} ${ctx.req.url}')
	}

	return true
}

// cors handles cross-origin requests by adding Access-Control-* headers to a
// preflight request and validating the headers of a cross-origin request.
// Example:
// ```v
// app.use(veb.cors[Context](veb.CorsOptions{
//     origins: ['*']
//     allowed_methods: [.get, .head, .patch, .put, .post, .delete]
// }))
// ```
pub fn cors[T](options CorsOptions) MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [options] [T](mut ctx T) bool {
			if ctx.req.method == .options { // preflight
				options.set_headers(mut ctx.Context)
				ctx.text('ok')
				return false
			} else {
				return options.validate_request(mut ctx.Context)
			}
		}
	}
}
