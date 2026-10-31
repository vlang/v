module veb

// body_limit returns middleware that answers a request whose body has more
// than `max_bytes` bytes with a 413 that names the limit, before the handler
// runs. Register it with `route_use` on the routes that need a lower limit
// than the rest of the app: a JSON API may take 100KB while the avatar upload
// beside it takes 5MB.
// ```v
// app.route_use('/api/profile', veb.body_limit[Context](100 * 1024))
// ```
//
// It is a per-route policy, not memory protection: before any middleware runs,
// veb has already read the whole body into memory and parsed its form or
// multipart data. The limit for that is `max_request_body_size` in `RunParams`
// (64 MiB by default), which answers a larger body with 413 and closes the
// connection. `body_limit` only has an effect when `max_bytes` is lower than
// `max_request_body_size`, and it does not close the connection.
//
// `max_bytes` of 0 refuses any non-empty body. A negative `max_bytes` panics.
pub fn body_limit[T](max_bytes int) MiddlewareOptions[T] {
	if max_bytes < 0 {
		panic('veb.body_limit: invalid max_bytes `${max_bytes}`, it should be 0 or a positive number of bytes')
	}
	return MiddlewareOptions[T]{
		handler: fn [max_bytes] [T](mut ctx T) bool {
			if ctx.req.data.len > max_bytes {
				ctx.res.set_status(.request_entity_too_large)
				ctx.text('request body of ${ctx.req.data.len} bytes exceeds the limit of ${max_bytes} bytes')
				return false
			}
			return true
		}
	}
}
