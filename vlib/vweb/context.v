module vweb

import net
import net.http

// The Context struct represents the Context which hold the HTTP request and response. 
// It has fields for the query, form, files.
pub struct Context {
mut:
	is_done bool
pub:
	// time.ticks() from start of vweb connection handle.
	// You can use it to determine how much time is spent on your request.
	request_start    u64

	// HTTP Request
	request http.Request
pub mut:
	// TCP connection to client, feel free to use it.
	// But beware, do not store it for further use, after request processing vweb will close connection.
	conn              net.TcpConn

	// Map containing query params for the route.
	// Example: `http://localhost:3000/index?q=vpm&order_by=desc => { 'q': 'vpm', 'order_by': 'desc' }`
	query             map[string]string

	// Multipart-from fields.
	form              map[string]string

	// Files from multipart-form.
	files             map[string][]http.FileData
	
	// HTTP Response, it will be sended on ctx.send().
	// Feel free to change it yourself.
	response http.Response
}

// Defining this method is optional. 
// This method called before every request (aka middleware).
// Probably you can use it for check user session cookie or add header.
pub fn (mut ctx Context) before_request() {}

// Sets the response status.
pub fn (mut ctx Context) set_status(code http.Status) {
	ctx.response.set_status(code)
}

// Sends status `code` to client.
pub fn (mut ctx Context) send_status(code http.Status) Result {
	ctx.set_status(code)
	return ctx.send()
}

// Sets the response content type to `mime_type`.
pub fn (mut ctx Context) set_content_type(mime_type string) {
	ctx.response.header.set(.content_type, mime_type)
}

// Returns request body.
pub fn (ctx Context) body() string {
	return ctx.request.data
}

// Sets response body to `payload`.
pub fn (mut ctx Context) set_body(payload string) {
	ctx.response.text = payload
}

// Sends html `payload` with status `code`.
pub fn (mut ctx Context) html(code http.Status, payload string) Result {
	ctx.set_content_type('text/html')
	ctx.set_body(payload)
	return ctx.send()
}

// Sends text `payload` with status `code`.
pub fn (mut ctx Context) text(code http.Status, payload string) Result {
	ctx.set_content_type('text/plain')
	ctx.set_body(payload)
	return ctx.send()
}

// Sends json `payload` with status `code`.
pub fn (mut ctx Context) json(code http.Status, payload string) Result {
	ctx.set_content_type('application/json')
	ctx.set_body(payload)
	return ctx.send()
}

// Redirect client to `url`.
// Sets status to 302 (.found) and adds `location` to header.
pub fn (mut ctx Context) redirect(url string) Result {
	ctx.set_status(.found)
	ctx.response.header.add(.location, url)
	return ctx.send()
}

// Returns the ip address from the current user.
pub fn (ctx Context) ip() string {
	mut ip := ctx.request.header.get(.x_forwarded_for) or { '' }
	
	if ip == '' {
		ip = ctx.request.header.get_custom('X-Real-Ip') or { '' }
	}

	if ip.contains(',') {
		ip = ip.all_before(',')
	}
	
	if ip == '' {
		ip = ctx.conn.peer_ip() or { '' }
	}
	return ip
}

// Gets a cookie from request by a key.
pub fn (ctx &Context) get_cookie(key string) ?string {
	return ctx.request.cookies[key] or {
		return error('Cookie not found')
	}
}

// Sets response cookie.
pub fn (mut ctx Context) set_cookie(cookie http.Cookie) {
	ctx.response.header.add('Set-Cookie', cookie.str())
}

// Send response to client.
// After the method is called, the context is marked as done, so further calls will be ignored.
pub fn (mut ctx Context) send() Result {
	ctx.send_response()
	return Result{}
}

fn (ctx Context) mark_as_done() ? {
	if ctx.is_done {
		return error("already done")
	}
	ctx.is_done = true
}

// The actual method that sends the response to the client, 
// none of the methods above check the result of this method, 
// more you know it returns the success of the sending.
fn (mut ctx Context) send_response() bool {
	ctx.mark_as_done() or {
		return false
	}

	ctx.response.header.set(.content_length, ctx.response.text.len.str())
	ctx.conn.write(ctx.response.bytes()) or { return false }
	return true
}

