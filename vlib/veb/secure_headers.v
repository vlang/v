module veb

// SecureHeadersOptions configures the response headers the `secure_headers`
// middleware sets on every response. Every field ships with a safe default,
// and setting a field to `''` disables that header. The defaults follow what
// Echo's `Secure` middleware and Fiber's `Helmet` integration send; the one
// deliberate omission is `X-XSS-Protection`, which modern guidance says not to
// send because content sniffing is already off and the header can re-enable
// attacks in old browsers that still honour it.
@[params]
pub struct SecureHeadersOptions {
pub:
	// content_security_policy is the `Content-Security-Policy` value. The
	// default allows only resources from the same origin.
	content_security_policy string = "default-src 'self'"
	// strict_transport_security is the `Strict-Transport-Security` value. It
	// only takes effect over HTTPS; disable it while developing over plain
	// HTTP if the browser has pinned an earlier value.
	strict_transport_security string = 'max-age=31536000; includeSubDomains'
	// x_frame_options is the `X-Frame-Options` value.
	x_frame_options string = 'SAMEORIGIN'
	// x_content_type_options is the `X-Content-Type-Options` value.
	x_content_type_options string = 'nosniff'
	// referrer_policy is the `Referrer-Policy` value.
	referrer_policy string = 'strict-origin-when-cross-origin'
}

// set_headers writes the configured security headers onto the response. It
// replaces any value already set, so a handler that sets e.g. its own
// `Content-Security-Policy` keeps it and the middleware only fills the rest.
pub fn (options &SecureHeadersOptions) set_headers(mut ctx Context) {
	if options.content_security_policy != '' {
		ctx.res.header.set_custom('Content-Security-Policy', options.content_security_policy) or {}
	}
	if options.strict_transport_security != '' {
		ctx.res.header.set_custom('Strict-Transport-Security', options.strict_transport_security) or {}
	}
	if options.x_frame_options != '' {
		ctx.res.header.set_custom('X-Frame-Options', options.x_frame_options) or {}
	}
	if options.x_content_type_options != '' {
		ctx.res.header.set_custom('X-Content-Type-Options', options.x_content_type_options) or {}
	}
	if options.referrer_policy != '' {
		ctx.res.header.set_custom('Referrer-Policy', options.referrer_policy) or {}
	}
}

// secure_headers returns middleware that sets security headers on every
// response, then lets the request continue. Register it before any route that
// serves a browser, and before compression so the headers travel with the
// compressed body.
// Usage example:
// ```v
// app.use(veb.secure_headers[Context]())
// ```
pub fn secure_headers[T](options SecureHeadersOptions) MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [options] [T](mut ctx T) bool {
			options.set_headers(mut ctx.Context)
			return true
		}
	}
}
