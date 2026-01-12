module jsonrpc

// EventInterceptor called on `jsonrpc.dispatch_event`
pub type EventInterceptor = fn (name string, data string)

// EncodedRequestInterceptor called on `jsonrpc.intercept_encoded_request`
pub type EncodedRequestInterceptor = fn (req []u8) !

// RequestInterceptor called on `jsonrpc.intercept_request`
pub type RequestInterceptor = fn (req &Request) !

// ResponseInterceptor called on `jsonrpc.intercept_response`
pub type ResponseInterceptor = fn (resp &Response)

// EncodedResponseInterceptor called on `jsonrpc.intercept_encoded_response`
pub type EncodedResponseInterceptor = fn (resp []u8)

// Interceptors collection of all supported interceptors to be called on events
pub struct Interceptors {
pub mut:
	event            []EventInterceptor
	encoded_request  []EncodedRequestInterceptor
	request          []RequestInterceptor
	response         []ResponseInterceptor
	encoded_response []EncodedResponseInterceptor
}

// dispatch_event sends `event_name` and `data` to provided `jsonrpc.EventInterceptor`s
pub fn dispatch_event(ints []EventInterceptor, event_name string, data string) {
	for i in ints {
		i(event_name, data)
	}
}

// intercept_encoded_request sends raw request data before attempting
// to decode it into `jsonrpc.Request` to provided `jsonrpc.EncodedRequestInterceptor`s
pub fn intercept_encoded_request(ints []EncodedRequestInterceptor, req []u8) ! {
	for i in ints {
		i(req)!
	}
}

// intercept_request sends decoded `jsonrpc.Request` to provided `jsonrpc.RequestInterceptor`s
pub fn intercept_request(ints []RequestInterceptor, req &Request) ! {
	for i in ints {
		i(req)!
	}
}

// intercept_response sends decoded `jsonrpc.Response` to provided `jsonrpc.ResponseInterceptor`s
pub fn intercept_response(ints []ResponseInterceptor, resp &Response) {
	for i in ints {
		i(resp)
	}
}

// intercept_encoded_response sends raw encoded data representing
// `jsonrpc.Response` to provided `jsonrpc.EncodedResponseInterceptor`s
pub fn intercept_encoded_response(ints []EncodedResponseInterceptor, resp []u8) {
	for i in ints {
		i(resp)
	}
}

// is_interceptor_enabled checks if interceptor of provided type is enabled on `jsonrpc.Server` 
pub fn (s &Server) is_interceptor_enabled[T]() bool {
	s.get_interceptor[T]() or { return false }
	return true
}

// get_interceptor tries to find and return interceptor of provided type from `jsonrpc.Interceptors` 
pub fn (i Interceptors) get_interceptor[T]() ?&T {
	for inter in i.event {
		if inter is T {
			return inter
		}
	}
	for inter in i.encoded_request {
		if inter is T {
			return inter
		}
	}
	for inter in i.request {
		if inter is T {
			return inter
		}
	}
	for inter in i.response {
		if inter is T {
			return inter
		}
	}
	for inter in i.encoded_response {
		if inter is T {
			return inter
		}
	}

	return none
}
