module jsonrpc

// EventInterceptor called on `jsonrpc.dispatch_event`
pub type EventInterceptor =	fn(name string, data string)
// EncodedRequestInterceptor called on `jsonrpc.intercept_encoded_request`
pub type EncodedRequestInterceptor = fn(req []u8) !
// RequestInterceptor called on `jsonrpc.intercept_request`
pub type RequestInterceptor = fn(req &Request) !
// ResponseInterceptor called on `jsonrpc.intercept_response`
pub type ResponseInterceptor = fn(resp &Response)
// EncodedResponseInterceptor called on `jsonrpc.intercept_encoded_response`
pub type EncodedResponseInterceptor = fn(resp []u8)


// Interceptors collection of all supported interceptors to be called on events
pub struct Interceptors {
pub mut:
	event []EventInterceptor
	encoded_request []EncodedRequestInterceptor
	request []RequestInterceptor
	response []ResponseInterceptor
	encoded_response []EncodedResponseInterceptor
}

pub fn dispatch_event(ints []EventInterceptor, event_name string, data string) {
	for i in ints {
		i(event_name, data)
	}
}

pub fn intercept_encoded_request(ints []EncodedRequestInterceptor, req []u8) ! {
	for i in ints {
		i(req)!
	}
}

pub fn intercept_request(ints []RequestInterceptor, req &Request) ! {
	for i in ints {
		i(req)!
	}
}

pub fn intercept_response(ints []ResponseInterceptor, resp &Response) {
	for i in ints {
		i(resp)
	}
}

pub fn intercept_encoded_response(ints []EncodedResponseInterceptor, resp []u8) {
	for i in ints {
		i(resp)
	}
}

pub fn (s &Server) is_interceptor_enabled[T]() bool {
	s.get_interceptor[T]() or { return false }
	return true
}

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