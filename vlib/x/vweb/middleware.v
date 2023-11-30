module vweb

import compress.gzip

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
	handler fn (mut ctx T) bool @[required]
	after   bool
}

// string representation of Middleware
pub fn (m &Middleware[T]) str() string {
	return 'vweb.Middleware[${T.name}]{
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

// route_use registers a middlware handler for a specific route(s)
pub fn (mut m Middleware[T]) route_use(route string, options MiddlewareOptions[T]) {
	middleware := RouteMiddleware{
		url_parts: route.split('/').filter(it != '')
		handler: voidptr(options.handler)
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
// Example: app.use(vweb.encode_gzip[Context]())
pub fn encode_gzip[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after: true
		handler: fn [T](mut ctx T) bool {
			// TODO: compress file in streaming manner, or precompress them?
			if ctx.return_type == .file {
				return true
			}
			// first try compressions, because if it fails we can still send a response
			// before taking over the connection
			compressed := gzip.compress(ctx.res.body.bytes()) or {
				eprintln('[vweb] error while compressing with gzip: ${err.msg()}')
				return true
			}
			// enables us to have full controll over what response is send over the connection
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
// Example: app.use(vweb.decode_gzip[Context]())
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
