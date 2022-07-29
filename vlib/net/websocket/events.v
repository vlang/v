module websocket

// MessageEventHandler represents a callback on a new message
struct MessageEventHandler {
	handler  SocketMessageFn  // callback function
	handler2 SocketMessageFn2 // callback function with reference
	is_ref   bool    // true if has a reference object
	ref      voidptr // referenced object
}

// ErrorEventHandler represents a callback on error
struct ErrorEventHandler {
	handler  SocketErrorFn  // callback function
	handler2 SocketErrorFn2 // callback function with reference
	is_ref   bool    // true if has a reference object
	ref      voidptr // referenced object
}

// OpenEventHandler represents a callback when connection is opened
struct OpenEventHandler {
	handler  SocketOpenFn  // callback function
	handler2 SocketOpenFn2 // callback function with reference
	is_ref   bool    // true if has a reference object
	ref      voidptr // referenced object
}

// CloseEventHandler represents a callback on a closing event
struct CloseEventHandler {
	handler  SocketCloseFn  // callback function
	handler2 SocketCloseFn2 // callback function with reference
	is_ref   bool    // true if has a reference object
	ref      voidptr // referenced object
}

pub type AcceptClientFn = fn (mut c ServerClient) ?bool

pub type SocketMessageFn = fn (mut c Client, msg &Message) ?

pub type SocketMessageFn2 = fn (mut c Client, msg &Message, v voidptr) ?

pub type SocketErrorFn = fn (mut c Client, err string) ?

pub type SocketErrorFn2 = fn (mut c Client, err string, v voidptr) ?

pub type SocketOpenFn = fn (mut c Client) ?

pub type SocketOpenFn2 = fn (mut c Client, v voidptr) ?

pub type SocketCloseFn = fn (mut c Client, code int, reason string) ?

pub type SocketCloseFn2 = fn (mut c Client, code int, reason string, v voidptr) ?

// on_connect registers a callback when client connects to the server
pub fn (mut s Server) on_connect(fun AcceptClientFn) ? {
	if s.accept_client_callbacks.len > 0 {
		return error('only one callback can be registered for accept client')
	}
	s.accept_client_callbacks << fun
}

// on_message registers a callback on new messages
pub fn (mut s Server) on_message(fun SocketMessageFn) {
	s.message_callbacks << MessageEventHandler{
		handler: fun
	}
}

// on_message_ref registers a callback on new messages and provides a reference object
pub fn (mut s Server) on_message_ref(fun SocketMessageFn2, ref voidptr) {
	s.message_callbacks << MessageEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// on_close registers a callback on closed socket
pub fn (mut s Server) on_close(fun SocketCloseFn) {
	s.close_callbacks << CloseEventHandler{
		handler: fun
	}
}

// on_close_ref registers a callback on closed socket and provides a reference object
pub fn (mut s Server) on_close_ref(fun SocketCloseFn2, ref voidptr) {
	s.close_callbacks << CloseEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// on_message registers a callback on new messages
pub fn (mut ws Client) on_message(fun SocketMessageFn) {
	ws.message_callbacks << MessageEventHandler{
		handler: fun
	}
}

// on_message_ref registers a callback on new messages and provides a reference object
pub fn (mut ws Client) on_message_ref(fun SocketMessageFn2, ref voidptr) {
	ws.message_callbacks << MessageEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// on_error registers a callback on errors
pub fn (mut ws Client) on_error(fun SocketErrorFn) {
	ws.error_callbacks << ErrorEventHandler{
		handler: fun
	}
}

// on_error_ref registers a callback on errors and provides a reference object
pub fn (mut ws Client) on_error_ref(fun SocketErrorFn2, ref voidptr) {
	ws.error_callbacks << ErrorEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// on_open registers a callback on successful opening the websocket
pub fn (mut ws Client) on_open(fun SocketOpenFn) {
	ws.open_callbacks << OpenEventHandler{
		handler: fun
	}
}

// on_open_ref registers a callback on successful opening the websocket
// and provides a reference object
pub fn (mut ws Client) on_open_ref(fun SocketOpenFn2, ref voidptr) {
	ws.open_callbacks << OpenEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// on_close registers a callback on closed socket
pub fn (mut ws Client) on_close(fun SocketCloseFn) {
	ws.close_callbacks << CloseEventHandler{
		handler: fun
	}
}

// on_close_ref registers a callback on closed socket and provides a reference object
pub fn (mut ws Client) on_close_ref(fun SocketCloseFn2, ref voidptr) {
	ws.close_callbacks << CloseEventHandler{
		handler2: fun
		ref: ref
		is_ref: true
	}
}

// send_connect_event invokes the on_connect callback
fn (mut s Server) send_connect_event(mut c ServerClient) ?bool {
	if s.accept_client_callbacks.len == 0 {
		// If no callback all client will be accepted
		return true
	}
	fun := s.accept_client_callbacks[0]
	res := fun(mut c)?
	return res
}

// send_message_event invokes the on_message callback
fn (mut ws Client) send_message_event(msg &Message) {
	ws.debug_log('sending on_message event')
	for ev_handler in ws.message_callbacks {
		if !ev_handler.is_ref {
			ev_handler.handler(mut ws, msg) or { ws.logger.error('send_message_event error: $err') }
		} else {
			ev_handler.handler2(mut ws, msg, ev_handler.ref) or {
				ws.logger.error('send_message_event error: $err')
			}
		}
	}
}

// send_error_event invokes the on_error callback
fn (mut ws Client) send_error_event(error string) {
	ws.debug_log('sending on_error event')
	for ev_handler in ws.error_callbacks {
		if !ev_handler.is_ref {
			ev_handler.handler(mut ws, error) or {
				ws.logger.error('send_error_event error: $error, err: $err')
			}
		} else {
			ev_handler.handler2(mut ws, error, ev_handler.ref) or {
				ws.logger.error('send_error_event error: $error, err: $err')
			}
		}
	}
}

// send_close_event invokes the on_close callback
fn (mut ws Client) send_close_event(code int, reason string) {
	ws.debug_log('sending on_close event')
	for ev_handler in ws.close_callbacks {
		if !ev_handler.is_ref {
			ev_handler.handler(mut ws, code, reason) or {
				ws.logger.error('send_close_event error: $err')
			}
		} else {
			ev_handler.handler2(mut ws, code, reason, ev_handler.ref) or {
				ws.logger.error('send_close_event error: $err')
			}
		}
	}
}

// send_open_event invokes the on_open callback
fn (mut ws Client) send_open_event() {
	ws.debug_log('sending on_open event')
	for ev_handler in ws.open_callbacks {
		if !ev_handler.is_ref {
			ev_handler.handler(mut ws) or { ws.logger.error('send_open_event error: $err') }
		} else {
			ev_handler.handler2(mut ws, ev_handler.ref) or {
				ws.logger.error('send_open_event error: $err')
			}
		}
	}
}
