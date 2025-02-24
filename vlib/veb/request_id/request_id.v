module request_id

import veb
import rand

@[params]
pub struct Config {
pub:
	// Next defines a function to skip this middleware when returned true.
	next ?fn (ctx &veb.Context) bool
	// Generator defines a function to generate the unique identifier.
	generator fn () string = rand.uuid_v4
	// Header is the header key where to get/set the unique request ID.
	header string = 'X-Request-ID'
	// Allow empty sets whether to allow empty request IDs
	allow_empty bool
	// Force determines whether to always generate a new ID even if one exists
	force bool
}

@[noinit]
pub struct RequestIdContext {
pub mut:
	request_id_config Config
	request_id_exempt bool
	request_id        string
}

// get_request_id returns the current request ID
pub fn (ctx &RequestIdContext) get_request_id() string {
	return ctx.request_id
}

// middleware returns a handler that you can use with veb's middleware
pub fn middleware[T](config Config) veb.MiddlewareOptions[T] {
	return veb.MiddlewareOptions[T]{
		after:   false
		handler: fn [config] [T](mut ctx T) bool {
			if ctx.request_id_exempt {
				return true
			}

			// Don't execute middleware if Next returns true.
			if next := config.next {
				if next(ctx) {
					return true
				}
			}

			// Get existing ID from request
			mut rid := if !config.force {
				ctx.get_custom_header(config.header) or { '' }
			} else {
				''
			}

			// Generate new ID if needed
			if rid == '' || config.force {
				rid = config.generator()
			}

			// Set ID to response header if we have one
			if rid != '' {
				ctx.set_custom_header(config.header, rid)
				ctx.request_id = rid
			}

			// Store config
			ctx.request_id_config = config

			return true
		}
	}
}
