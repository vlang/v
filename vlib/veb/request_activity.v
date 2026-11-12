module veb

import net.http

// Optional process-wide request activity hooks. Unlike after-middleware, these
// are balanced around static serving, routing errors and early returns too.
interface RequestActivityApp {
mut:
	before_handle_request(req http.Request)
	after_handle_request(req http.Request)
}
