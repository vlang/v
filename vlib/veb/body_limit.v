module veb

// body_limit returns middleware that refuses request bodies larger than
// `max_bytes` with a 413 before the handler runs. The global
// `max_request_buffer_size` guards the connection; this guards the route: an
// avatar upload may take 5MB while the JSON API beside it takes 100KB, and a
// legitimate client that uploads too much deserves a 413 naming the limit,
// not a dropped connection.
//
// Register it on the routes it protects, not globally: a single global limit
// is what `max_request_buffer_size` already is.
// ```v
// app.route_use('/upload', veb.body_limit[Context](5 * 1024 * 1024))
// ```
pub fn body_limit[T](max_bytes i64) MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [max_bytes] [T](mut ctx T) bool {
			if i64(ctx.req.data.len) > max_bytes {
				ctx.res.set_status(.request_entity_too_large)
				ctx.text('request body of ${ctx.req.data.len} bytes exceeds the limit of ${max_bytes} bytes')
				return false
			}
			return true
		}
	}
}
