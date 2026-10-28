module veb

import compress.gzip
import compress.zstd
import net.http

pub type MiddlewareHandler[T] = fn (mut T) bool

interface MiddlewareApp {
	get_handlers_for_route(route_path string) []RouteMiddleware
	get_handlers_for_route_after(route_path string) []RouteMiddleware
	get_global_handlers() []voidptr
	get_global_handlers_after() []voidptr
}

struct RouteMiddleware {
	url_parts []string
	methods   []http.Method
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
	methods []http.Method
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
		methods:   options.methods.clone()
		handler:   voidptr(options.handler)
	}

	if options.after {
		m.route_handlers_after << middleware
	} else {
		m.route_handlers << middleware
	}
}

fn (m &Middleware[T]) get_handlers_for_route(route_path string) []RouteMiddleware {
	mut handlers := []RouteMiddleware{}
	route_parts := route_path.split('/').filter(it != '')

	for handler in m.route_handlers {
		if _ := route_matches(route_parts, handler.url_parts) {
			handlers << handler
		} else if handler.url_parts.len == 0 && route_path == '/index' {
			handlers << handler
		}
	}

	return handlers
}

fn (m &Middleware[T]) get_handlers_for_route_after(route_path string) []RouteMiddleware {
	mut handlers := []RouteMiddleware{}
	route_parts := route_path.split('/').filter(it != '')

	for handler in m.route_handlers_after {
		if _ := route_matches(route_parts, handler.url_parts) {
			handlers << handler
		} else if handler.url_parts.len == 0 && route_path == '/index' {
			handlers << handler
		}
	}

	return handlers
}

fn (m &Middleware[T]) get_global_handlers() []voidptr {
	return m.global_handlers
}

fn (m &Middleware[T]) get_global_handlers_after() []voidptr {
	return m.global_handlers_after
}

fn app_route_handlers[A](app &A, route_path string) []RouteMiddleware {
	$if A is $struct {
		$for field in A.fields {
			$if field.is_embed {
				$if field.name == 'Middleware' {
					return app.$(field.name).get_handlers_for_route(route_path)
				} $else $if field.typ is $struct {
					handlers := app_route_handlers(app.$(field.name), route_path)
					if handlers.len > 0 {
						return handlers
					}
				}
			}
		}
	}
	return []RouteMiddleware{}
}

fn app_route_handlers_after[A](app &A, route_path string) []RouteMiddleware {
	$if A is $struct {
		$for field in A.fields {
			$if field.is_embed {
				$if field.name == 'Middleware' {
					return app.$(field.name).get_handlers_for_route_after(route_path)
				} $else $if field.typ is $struct {
					handlers := app_route_handlers_after(app.$(field.name), route_path)
					if handlers.len > 0 {
						return handlers
					}
				}
			}
		}
	}
	return []RouteMiddleware{}
}

fn app_global_handlers[A](app &A) []voidptr {
	$if A is $struct {
		$for field in A.fields {
			$if field.is_embed {
				$if field.name == 'Middleware' {
					return app.$(field.name).get_global_handlers()
				} $else $if field.typ is $struct {
					handlers := app_global_handlers(app.$(field.name))
					if handlers.len > 0 {
						return handlers
					}
				}
			}
		}
	}
	return []voidptr{}
}

fn app_global_handlers_after[A](app &A) []voidptr {
	$if A is $struct {
		$for field in A.fields {
			$if field.is_embed {
				$if field.name == 'Middleware' {
					return app.$(field.name).get_global_handlers_after()
				} $else $if field.typ is $struct {
					handlers := app_global_handlers_after(app.$(field.name))
					if handlers.len > 0 {
						return handlers
					}
				}
			}
		}
	}
	return []voidptr{}
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

fn route_middleware_matches_method(route_middleware RouteMiddleware, request_method http.Method) bool {
	return route_middleware.methods.len == 0 || request_method in route_middleware.methods
}

fn get_handlers_for_method(route_middlewares []RouteMiddleware, request_method http.Method) []voidptr {
	mut handlers := []voidptr{}
	for route_middleware in route_middlewares {
		if route_middleware_matches_method(route_middleware, request_method) {
			handlers << route_middleware.handler
		}
	}
	return handlers
}

// Compression encoding types for HTTP responses
enum ContentEncoding {
	gzip
	zstd
}

// min_compression_body_bytes is the smallest response body eligible for
// dynamic compression. Below this size the framing overhead exceeds any
// savings: compressed output is usually larger than the input, and the CPU
// cost buys nothing. (Ported from viltrum's gzip_min_bytes.)
const min_compression_body_bytes = 256

// compressible_content_type reports whether responses with the given
// Content-Type value are worth compressing. Already-compressed formats
// (images, audio, video, archives) are skipped; an empty value is allowed so
// untyped responses keep the previous behavior.
fn compressible_content_type(content_type string) bool {
	ct := content_type.all_before(';').trim_space().to_lower()
	if ct == '' {
		return true
	}
	if ct.starts_with('text/') {
		return true
	}
	for token in ['json', 'javascript', 'ecmascript', 'xml', 'svg'] {
		if ct.contains(token) {
			return true
		}
	}
	return false
}

// response_allows_body reports whether the status code permits a message body.
fn response_allows_body(status_code int) bool {
	if status_code >= 100 && status_code < 200 {
		return false
	}
	return status_code != 204 && status_code != 205 && status_code != 304
}

// qvalue_is_zero reports whether an Accept-Encoding q-value refuses the
// coding. Only the zero value matters here: any positive q-value is
// acceptable, and zstd is always preferred over gzip when both are offered.
fn qvalue_is_zero(qval string) bool {
	t := qval.trim_space()
	if t == '0' {
		return true
	}
	if t.starts_with('0.') {
		for c in t[2..] {
			if c != `0` {
				return false
			}
		}
		return true
	}
	return false
}

// client_accepts_encoding reports whether an Accept-Encoding header value
// offers `encoding`. An explicit entry wins over `*`, an explicit `;q=0`
// refuses the coding, matching is case-insensitive, and a missing header
// accepts nothing.
fn client_accepts_encoding(accept_encoding string, encoding string) bool {
	mut star_q_zero := true
	mut star_seen := false
	for part in accept_encoding.split(',') {
		mut coding := part.trim_space().to_lower()
		mut refused := false
		if idx := coding.index(';') {
			params := coding[idx + 1..]
			coding = coding[..idx].trim_space()
			for param in params.split(';') {
				p := param.replace(' ', '').replace('\t', '')
				if p.starts_with('q=') && qvalue_is_zero(p[2..]) {
					refused = true
					break
				}
			}
		}
		if coding == encoding {
			return !refused
		}
		if coding == '*' {
			star_seen = true
			star_q_zero = refused
		}
	}
	return star_seen && !star_q_zero
}

// send_compressed_response compresses the response body and updates the response.
// Returns true if compression should be skipped, false if compression was applied.
//
// Compression is skipped when it cannot pay for itself or would be wrong:
// already-compressed or streaming file responses, statuses without a body,
// bodies below min_compression_body_bytes, incompressible media types, clients
// that did not offer the encoding (q-values honoured), and output that would
// be larger than the input.
fn send_compressed_response(mut ctx Context, encoding ContentEncoding) bool {
	if should_skip_compression(ctx) {
		return true
	}
	if !response_allows_body(ctx.res.status_code) {
		return true
	}
	body := ctx.res.body
	if body.len < min_compression_body_bytes {
		return true
	}
	content_type := ctx.res.header.get(.content_type) or { '' }
	// Caches must not serve a compressed variant to clients that did not ask
	// for one; advertise the variance even when this response stays plaintext.
	add_accept_encoding_vary(mut ctx)
	if !compressible_content_type(content_type) {
		return true
	}
	encoding_name := if encoding == .zstd { 'zstd' } else { 'gzip' }
	accept_encoding := ctx.req.header.get(.accept_encoding) or { '' }
	if !client_accepts_encoding(accept_encoding, encoding_name) {
		return true
	}
	compressed, _ := match encoding {
		.zstd {
			data := zstd.compress(body.bytes()) or {
				eprintln('[veb] error while compressing with zstd: ${err.msg()}')
				return true
			}
			data, 'zstd'
		}
		.gzip {
			data := gzip.compress(body.bytes()) or {
				eprintln('[veb] error while compressing with gzip: ${err.msg()}')
				return true
			}
			data, 'gzip'
		}
	}
	// Compression that does not shrink the body only wastes bytes on the wire.
	if compressed.len >= body.len {
		return true
	}

	// Set HTTP headers for compressed content
	ctx.res.header.add(.content_encoding, encoding_name)
	if etag := ctx.res.header.get(.etag) {
		if !etag.starts_with('W/') {
			ctx.res.header.set(.etag, 'W/${etag}')
		}
	}

	// Replace the response body with the compressed data and update Content-Length.
	// The normal response path will handle sending it.
	ctx.res.body = compressed.bytestr()
	ctx.res.header.set(.content_length, compressed.len.str())
	ctx.already_compressed = true

	return false
}

// should_skip_compression checks if compression should be skipped for this context.
fn should_skip_compression(ctx Context) bool {
	// Skip if already compressed (optimization for static files compressed in send_file)
	if ctx.already_compressed {
		return true
	}
	// Skip compression for files in streaming mode (no takeover)
	// Files in takeover mode (small files loaded in memory) are compressed
	if ctx.return_type == .file && ctx.takeover_mode == .none {
		return true
	}
	return false
}

fn add_accept_encoding_vary(mut ctx Context) {
	for value in ctx.res.header.values(.vary) {
		for field in value.split(',') {
			if field.trim_space().to_lower() in ['*', 'accept-encoding'] {
				return
			}
		}
	}
	ctx.res.header.add(.vary, 'Accept-Encoding')
}

// encode_gzip adds gzip encoding to the HTTP Response body.
// This middleware compresses dynamic routes and static files loaded in memory (takeover mode).
// Static files in streaming mode are compressed by send_file() when static compression is enabled,
// and this middleware skips them to avoid double compression (via the already_compressed flag).
// Register this middleware as last!
// Usage example: app.use(veb.encode_gzip[Context]())
pub fn encode_gzip[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [T](mut ctx T) bool {
			if should_skip_compression(ctx.Context) {
				return true
			}
			return send_compressed_response(mut ctx.Context, .gzip)
		}
	}
}

// encode_zstd adds zstd encoding to the HTTP Response body.
// This middleware compresses dynamic routes and static files loaded in memory (takeover mode).
// Static files in streaming mode are compressed by send_file() when static compression is enabled,
// and this middleware skips them to avoid double compression (via the already_compressed flag).
// Register this middleware as last!
// Usage example: app.route_use('/api', veb.encode_zstd[Context]())
pub fn encode_zstd[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [T](mut ctx T) bool {
			if should_skip_compression(ctx.Context) {
				return true
			}
			return send_compressed_response(mut ctx.Context, .zstd)
		}
	}
}

// encode_auto adds automatic content encoding (zstd or gzip) based on the client's Accept-Encoding header.
// This middleware checks the Accept-Encoding header and compresses with zstd if supported, otherwise gzip.
// Static files in streaming mode are compressed by send_file() when static compression is enabled,
// and this middleware skips them to avoid double compression (via the already_compressed flag).
// Register this middleware as last!
// Usage example: app.use(veb.encode_auto[Context]())
pub fn encode_auto[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [T](mut ctx T) bool {
			if should_skip_compression(ctx.Context) {
				return true
			}

			// Check Accept-Encoding header to determine best compression,
			// honouring q-values (`gzip;q=0` refuses compression).
			accept_encoding := ctx.req.header.get(.accept_encoding) or { '' }

			// Try zstd first (better compression ratio), fallback to gzip
			if client_accepts_encoding(accept_encoding, 'zstd') {
				return send_compressed_response(mut ctx.Context, .zstd)
			}
			if client_accepts_encoding(accept_encoding, 'gzip') {
				return send_compressed_response(mut ctx.Context, .gzip)
			}

			// Apply eligibility and Vary to the identity representation too.
			return send_compressed_response(mut ctx.Context, .gzip)
		}
	}
}

// decode_gzip decodes the body of a gzip'ed HTTP request.
// Register this middleware before you do anything with the request body!
// Usage example: app.use(veb.decode_gzip[Context]())
pub fn decode_gzip[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [T](mut ctx T) bool {
			if encoding := ctx.req.header.get(.content_encoding) {
				if encoding == 'gzip' {
					decompressed := gzip.decompress(ctx.req.data.bytes()) or {
						ctx.request_error('invalid gzip encoding')
						return false
					}
					ctx.req.data = decompressed.bytestr()
				}
			}
			return true
		}
	}
}

// decode_zstd decodes the body of a zstd-compressed HTTP request.
// Register this middleware before you do anything with the request body!
// Usage example: app.use(veb.decode_zstd[Context]())
pub fn decode_zstd[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [T](mut ctx T) bool {
			if encoding := ctx.req.header.get(.content_encoding) {
				if encoding == 'zstd' {
					decompressed := zstd.decompress(ctx.req.data.bytes()) or {
						ctx.request_error('invalid zstd encoding')
						return false
					}
					ctx.req.data = decompressed.bytestr()
				}
			}
			return true
		}
	}
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
	// allowed CORS request header names; `Access-Control-Allow-Headers`.
	// Compared case-insensitively with `Access-Control-Request-Headers`.
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
// Header allow-lists apply to names advertised in `Access-Control-Request-Headers`.
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
		allowed_headers := options.allowed_headers.map(it.to_lower())
		requested_headers := ctx.req.header.get(.access_control_request_headers) or { '' }
		for name in requested_headers.split(',') {
			header := name.trim_space()
			if header.len > 0 && header.to_lower() !in allowed_headers {
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
// Usage example:
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
