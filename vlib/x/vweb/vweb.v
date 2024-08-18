@[deprecated: '`x.vweb` is now `veb`. The module is no longer experimental. Simply import veb instead of x.vweb']
module vweb

import io
import net
import net.http
import net.urllib
import os
import time
import strings
import picoev

// max read and write limits in bytes
const max_read = 8096
const max_write = 8096 * 2

// A type which doesn't get filtered inside templates
pub type RawHtml = string

// A dummy structure that returns from routes to indicate that you actually sent something to a user
@[noinit]
pub struct Result {}

// no_result does nothing, but returns `vweb.Result`. Only use it when you are sure
// a response will be send over the connection, or in combination with `Context.takeover_conn`
pub fn no_result() Result {
	return Result{}
}

pub const methods_with_form = [http.Method.post, .put, .patch]

pub const headers_close = http.new_custom_header_from_map({
	'Server': 'VWeb'
}) or { panic('should never fail') }

pub const http_302 = http.new_response(
	status: .found
	body:   '302 Found'
	header: headers_close
)

pub const http_400 = http.new_response(
	status: .bad_request
	body:   '400 Bad Request'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_404 = http.new_response(
	status: .not_found
	body:   '404 Not Found'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_408 = http.new_response(
	status: .request_timeout
	body:   '408 Request Timeout'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_413 = http.new_response(
	status: .request_entity_too_large
	body:   '413 Request entity is too large'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_500 = http.new_response(
	status: .internal_server_error
	body:   '500 Internal Server Error'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const mime_types = {
	'.aac':    'audio/aac'
	'.abw':    'application/x-abiword'
	'.arc':    'application/x-freearc'
	'.avi':    'video/x-msvideo'
	'.azw':    'application/vnd.amazon.ebook'
	'.bin':    'application/octet-stream'
	'.bmp':    'image/bmp'
	'.bz':     'application/x-bzip'
	'.bz2':    'application/x-bzip2'
	'.cda':    'application/x-cdf'
	'.csh':    'application/x-csh'
	'.css':    'text/css'
	'.csv':    'text/csv'
	'.doc':    'application/msword'
	'.docx':   'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
	'.eot':    'application/vnd.ms-fontobject'
	'.epub':   'application/epub+zip'
	'.gz':     'application/gzip'
	'.gif':    'image/gif'
	'.htm':    'text/html'
	'.html':   'text/html'
	'.ico':    'image/vnd.microsoft.icon'
	'.ics':    'text/calendar'
	'.jar':    'application/java-archive'
	'.jpeg':   'image/jpeg'
	'.jpg':    'image/jpeg'
	'.js':     'text/javascript'
	'.json':   'application/json'
	'.jsonld': 'application/ld+json'
	'.mid':    'audio/midi audio/x-midi'
	'.midi':   'audio/midi audio/x-midi'
	'.mjs':    'text/javascript'
	'.mp3':    'audio/mpeg'
	'.mp4':    'video/mp4'
	'.mpeg':   'video/mpeg'
	'.mpkg':   'application/vnd.apple.installer+xml'
	'.odp':    'application/vnd.oasis.opendocument.presentation'
	'.ods':    'application/vnd.oasis.opendocument.spreadsheet'
	'.odt':    'application/vnd.oasis.opendocument.text'
	'.oga':    'audio/ogg'
	'.ogv':    'video/ogg'
	'.ogx':    'application/ogg'
	'.opus':   'audio/opus'
	'.otf':    'font/otf'
	'.png':    'image/png'
	'.pdf':    'application/pdf'
	'.php':    'application/x-httpd-php'
	'.ppt':    'application/vnd.ms-powerpoint'
	'.pptx':   'application/vnd.openxmlformats-officedocument.presentationml.presentation'
	'.rar':    'application/vnd.rar'
	'.rtf':    'application/rtf'
	'.sh':     'application/x-sh'
	'.svg':    'image/svg+xml'
	'.swf':    'application/x-shockwave-flash'
	'.tar':    'application/x-tar'
	'.tif':    'image/tiff'
	'.tiff':   'image/tiff'
	'.ts':     'video/mp2t'
	'.ttf':    'font/ttf'
	'.txt':    'text/plain'
	'.vsd':    'application/vnd.visio'
	'.wasm':   'application/wasm'
	'.wav':    'audio/wav'
	'.weba':   'audio/webm'
	'.webm':   'video/webm'
	'.webp':   'image/webp'
	'.woff':   'font/woff'
	'.woff2':  'font/woff2'
	'.xhtml':  'application/xhtml+xml'
	'.xls':    'application/vnd.ms-excel'
	'.xlsx':   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
	'.xml':    'application/xml'
	'.xul':    'application/vnd.mozilla.xul+xml'
	'.zip':    'application/zip'
	'.3gp':    'video/3gpp'
	'.3g2':    'video/3gpp2'
	'.7z':     'application/x-7z-compressed'
	'.m3u8':   'application/vnd.apple.mpegurl'
	'.vsh':    'text/x-vlang'
	'.v':      'text/x-vlang'
}

pub const max_http_post_size = 1024 * 1024

pub const default_port = 8080

struct Route {
	methods []http.Method
	path    string
	host    string
mut:
	middlewares       []voidptr
	after_middlewares []voidptr
}

// Generate route structs for an app
fn generate_routes[A, X](app &A) !map[string]Route {
	// Parsing methods attributes
	mut routes := map[string]Route{}
	$for method in A.methods {
		$if method.return_type is Result {
			http_methods, route_path, host := parse_attrs(method.name, method.attrs) or {
				return error('error parsing method attributes: ${err}')
			}

			mut route := Route{
				methods: http_methods
				path:    route_path
				host:    host
			}

			$if A is MiddlewareApp {
				route.middlewares = app.Middleware.get_handlers_for_route[X](route_path)
				route.after_middlewares = app.Middleware.get_handlers_for_route_after[X](route_path)
			}

			routes[method.name] = route
		}
	}
	return routes
}

// run - start a new VWeb server, listening to all available addresses, at the specified `port`
pub fn run[A, X](mut global_app A, port int) {
	run_at[A, X](mut global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

@[params]
pub struct RunParams {
pub:
	// use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
	family               net.AddrFamily = .ip6
	host                 string
	port                 int  = 8080
	show_startup_message bool = true
	timeout_in_seconds   int  = 30
}

struct FileResponse {
pub mut:
	open              bool
	file              os.File
	total             i64
	pos               i64
	should_close_conn bool
}

// close the open file and reset the struct to its default values
pub fn (mut fr FileResponse) done() {
	fr.open = false
	fr.file.close()
	fr.total = 0
	fr.pos = 0
	fr.should_close_conn = false
}

struct StringResponse {
pub mut:
	open              bool
	str               string
	pos               i64
	should_close_conn bool
}

// free the current string and reset the struct to its default values
@[manualfree]
pub fn (mut sr StringResponse) done() {
	sr.open = false
	sr.pos = 0
	sr.should_close_conn = false
	unsafe { sr.str.free() }
}

// EV context
struct RequestParams {
	global_app         voidptr
	controllers        []&ControllerPath
	routes             &map[string]Route
	timeout_in_seconds int
mut:
	// request body buffer
	buf &u8 = unsafe { nil }
	// idx keeps track of how much of the request body has been read
	// for each incomplete request, see `handle_conn`
	idx                 []int
	incomplete_requests []http.Request
	file_responses      []FileResponse
	string_responses    []StringResponse
}

// reset request parameters for `fd`:
// reset content-length index and the http request
pub fn (mut params RequestParams) request_done(fd int) {
	params.incomplete_requests[fd] = http.Request{}
	params.idx[fd] = 0
}

interface BeforeAcceptApp {
mut:
	before_accept_loop()
}

// run_at - start a new VWeb server, listening only on a specific address `host`, at the specified `port`
// Example: vweb.run_at(new_app(), vweb.RunParams{ host: 'localhost' port: 8099 family: .ip }) or { panic(err) }
@[direct_array_access; manualfree]
pub fn run_at[A, X](mut global_app A, params RunParams) ! {
	if params.port <= 0 || params.port > 65535 {
		return error('invalid port number `${params.port}`, it should be between 1 and 65535')
	}

	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!

	if params.show_startup_message {
		host := if params.host == '' { 'localhost' } else { params.host }
		println('[Vweb] Running app on http://${host}:${params.port}/')
	}
	flush_stdout()

	mut pico_context := &RequestParams{
		global_app:         unsafe { global_app }
		controllers:        controllers_sorted
		routes:             &routes
		timeout_in_seconds: params.timeout_in_seconds
	}

	pico_context.idx = []int{len: picoev.max_fds}
	// reserve space for read and write buffers
	pico_context.buf = unsafe { malloc_noscan(picoev.max_fds * vweb.max_read + 1) }
	defer {
		unsafe { free(pico_context.buf) }
	}
	pico_context.incomplete_requests = []http.Request{len: picoev.max_fds}
	pico_context.file_responses = []FileResponse{len: picoev.max_fds}
	pico_context.string_responses = []StringResponse{len: picoev.max_fds}

	mut pico := picoev.new(
		port:         params.port
		raw_cb:       ev_callback[A, X]
		user_data:    pico_context
		timeout_secs: params.timeout_in_seconds
		family:       params.family
		host:         params.host
	)!

	$if A is BeforeAcceptApp {
		global_app.before_accept_loop()
	}

	// Forever accept every connection that comes
	pico.serve()
}

@[direct_array_access]
fn ev_callback[A, X](mut pv picoev.Picoev, fd int, events int) {
	mut params := unsafe { &RequestParams(pv.user_data) }

	if events == picoev.picoev_timeout {
		$if trace_picoev_callback ? {
			eprintln('> request timeout on file descriptor ${fd}')
		}

		handle_timeout(mut pv, mut params, fd)
	} else if events == picoev.picoev_write {
		$if trace_picoev_callback ? {
			eprintln('> write event on file descriptor ${fd}')
		}

		if params.file_responses[fd].open {
			handle_write_file(mut pv, mut params, fd)
		} else if params.string_responses[fd].open {
			handle_write_string(mut pv, mut params, fd)
		} else {
			// This should never happen, but it does on pages, that refer to static resources,
			// in folders, added with `mount_static_folder_at`. See also
			// https://github.com/vlang/edu-platform/blob/0c203f0384cf24f917f9a7c9bb150f8d64aca00f/main.v#L92
			$if debug_ev_callback ? {
				eprintln('[vweb] error: write event on connection should be closed')
			}
			pv.close_conn(fd)
		}
	} else if events == picoev.picoev_read {
		$if trace_picoev_callback ? {
			eprintln('> read event on file descriptor ${fd}')
		}
		handle_read[A, X](mut pv, mut params, fd)
	} else {
		// should never happen
		eprintln('[vweb] error: invalid picoev event ${events}')
	}
}

fn handle_timeout(mut pv picoev.Picoev, mut params RequestParams, fd int) {
	mut conn := &net.TcpConn{
		sock:        net.tcp_socket_from_handle_raw(fd)
		handle:      fd
		is_blocking: false
	}

	fast_send_resp(mut conn, vweb.http_408) or {}
	pv.close_conn(fd)

	params.request_done(fd)
}

// handle_write_file reads data from a file and sends that data over the socket.
@[direct_array_access; manualfree]
fn handle_write_file(mut pv picoev.Picoev, mut params RequestParams, fd int) {
	mut bytes_to_write := int(params.file_responses[fd].total - params.file_responses[fd].pos)

	$if linux || freebsd {
		bytes_written := sendfile(fd, params.file_responses[fd].file.fd, bytes_to_write)
		params.file_responses[fd].pos += bytes_written
	} $else {
		if bytes_to_write > vweb.max_write {
			bytes_to_write = vweb.max_write
		}

		data := unsafe { malloc(bytes_to_write) }
		defer {
			unsafe { free(data) }
		}

		mut conn := &net.TcpConn{
			sock:        net.tcp_socket_from_handle_raw(fd)
			handle:      fd
			is_blocking: false
		}

		params.file_responses[fd].file.read_into_ptr(data, bytes_to_write) or {
			params.file_responses[fd].done()
			pv.close_conn(fd)
			return
		}
		actual_written := send_string_ptr(mut conn, data, bytes_to_write) or {
			params.file_responses[fd].done()
			pv.close_conn(fd)
			return
		}
		params.file_responses[fd].pos += actual_written
	}

	if params.file_responses[fd].pos == params.file_responses[fd].total {
		// file is done writing
		params.file_responses[fd].done()
		handle_complete_request(params.file_responses[fd].should_close_conn, mut pv, fd)
		return
	}
}

// handle_write_string reads data from a string and sends that data over the socket
@[direct_array_access]
fn handle_write_string(mut pv picoev.Picoev, mut params RequestParams, fd int) {
	mut bytes_to_write := int(params.string_responses[fd].str.len - params.string_responses[fd].pos)

	if bytes_to_write > vweb.max_write {
		bytes_to_write = vweb.max_write
	}

	mut conn := &net.TcpConn{
		sock:        net.tcp_socket_from_handle_raw(fd)
		handle:      fd
		is_blocking: false
	}

	// pointer magic to start at the correct position in the buffer
	data := unsafe { params.string_responses[fd].str.str + params.string_responses[fd].pos }
	actual_written := send_string_ptr(mut conn, data, bytes_to_write) or {
		params.string_responses[fd].done()
		pv.close_conn(fd)
		return
	}
	params.string_responses[fd].pos += actual_written
	if params.string_responses[fd].pos == params.string_responses[fd].str.len {
		// done writing
		params.string_responses[fd].done()
		pv.close_conn(fd)
		handle_complete_request(params.string_responses[fd].should_close_conn, mut pv,
			fd)
		return
	}
}

// handle_read reads data from the connection and if the request is complete
// it calls `handle_route` and closes the connection.
// If the request is not complete it stores the incomplete request in `params`
// and the connection stays open until it is ready to read again
@[direct_array_access; manualfree]
fn handle_read[A, X](mut pv picoev.Picoev, mut params RequestParams, fd int) {
	mut conn := &net.TcpConn{
		sock:        net.tcp_socket_from_handle_raw(fd)
		handle:      fd
		is_blocking: false
	}

	// cap the max_read to 8KB
	mut reader := io.new_buffered_reader(reader: conn, cap: vweb.max_read)
	defer {
		unsafe {
			reader.free()
		}
	}

	// take the previous incomplete request
	mut req := params.incomplete_requests[fd]

	// check if there is an incomplete request for this file descriptor
	if params.idx[fd] == 0 {
		// set the read and write timeout according to picoev settings when the
		// connection is first encountered
		conn.set_read_timeout(params.timeout_in_seconds)
		conn.set_write_timeout(params.timeout_in_seconds)
		// first time that this connection is being read from, so we parse the
		// request header first
		req = http.parse_request_head(mut reader) or {
			// Prevents errors from being thrown when BufferedReader is empty
			if err !is io.Eof {
				eprintln('[vweb] error parsing request: ${err}')
			}
			// the buffered reader was empty meaning that the client probably
			// closed the connection.
			pv.close_conn(fd)
			params.incomplete_requests[fd] = http.Request{}
			return
		}
		if reader.total_read >= vweb.max_read {
			// throw an error when the request header is larger than 8KB
			// same limit that apache handles
			eprintln('[vweb] error parsing request: too large')
			fast_send_resp(mut conn, vweb.http_413) or {}

			pv.close_conn(fd)
			params.incomplete_requests[fd] = http.Request{}
			return
		}
	}

	// check if the request has a body
	content_length := req.header.get(.content_length) or { '0' }
	if content_length.int() > 0 {
		mut max_bytes_to_read := vweb.max_read - reader.total_read
		mut bytes_to_read := content_length.int() - params.idx[fd]
		// cap the bytes to read to 8KB for the body, including the request headers if any
		if bytes_to_read > vweb.max_read - reader.total_read {
			bytes_to_read = vweb.max_read - reader.total_read
		}

		mut buf_ptr := params.buf
		unsafe {
			buf_ptr += fd * vweb.max_read // pointer magic
		}
		// convert to []u8 for BufferedReader
		mut buf := unsafe { buf_ptr.vbytes(max_bytes_to_read) }

		n := reader.read(mut buf) or {
			eprintln('[vweb] error parsing request: ${err}')
			pv.close_conn(fd)
			params.incomplete_requests[fd] = http.Request{}
			params.idx[fd] = 0
			return
		}

		// there is no more data to be sent, but it is less than the Content-Length header
		// so it is a mismatch of body length and content length.
		// Or if there is more data received then the Content-Length header specified
		if (n == 0 && params.idx[fd] != 0) || params.idx[fd] + n > content_length.int() {
			fast_send_resp(mut conn, http.new_response(
				status: .bad_request
				body:   'Mismatch of body length and Content-Length header'
				header: http.new_header(
					key:   .content_type
					value: 'text/plain'
				).join(vweb.headers_close)
			)) or {}

			pv.close_conn(fd)
			params.incomplete_requests[fd] = http.Request{}
			params.idx[fd] = 0
			return
		} else if n < bytes_to_read || params.idx[fd] + n < content_length.int() {
			// request is incomplete wait until the socket becomes ready to read again
			params.idx[fd] += n
			// TODO: change this to a memcpy function?
			req.data += buf[0..n].bytestr()
			params.incomplete_requests[fd] = req
			return
		} else {
			// request is complete: n = bytes_to_read
			params.idx[fd] += n
			req.data += buf[0..n].bytestr()
		}
	}

	defer {
		params.request_done(fd)
	}

	if completed_context := handle_request[A, X](mut conn, req, params) {
		if completed_context.takeover {
			// the connection should be kept open, but removed from the picoev loop.
			// This way vweb can continue handling other connections and the user can
			// keep the connection open indefinitely
			pv.delete(fd)
			return
		}

		// TODO: At this point the Context can safely be freed when this function returns.
		// The user will have to clone the context if the context object should be kept.
		// defer {
		// 	completed_context.free()
		// }

		match completed_context.return_type {
			.normal {
				// small optimization: if the response is small write it immediately
				// the socket is most likely able to write all the data without blocking.
				// See Context.send_file for why we use max_read instead of max_write.
				if completed_context.res.body.len < vweb.max_read {
					fast_send_resp(mut conn, completed_context.res) or {}
					handle_complete_request(completed_context.client_wants_to_close, mut
						pv, fd)
				} else {
					params.string_responses[fd].open = true
					params.string_responses[fd].str = completed_context.res.body
					res := pv.add(fd, picoev.picoev_write, params.timeout_in_seconds,
						picoev.raw_callback)
					// picoev error
					if res == -1 {
						// should not happen
						params.string_responses[fd].done()
						fast_send_resp(mut conn, vweb.http_500) or {}
						handle_complete_request(completed_context.client_wants_to_close, mut
							pv, fd)
						return
					}
					// no errors we can send the HTTP headers
					fast_send_resp_header(mut conn, completed_context.res) or {}
				}
			}
			.file {
				// save file information
				length := completed_context.res.header.get(.content_length) or {
					fast_send_resp(mut conn, vweb.http_500) or {}
					return
				}
				params.file_responses[fd].total = length.i64()
				params.file_responses[fd].file = os.open(completed_context.return_file) or {
					// Context checks if the file is valid, so this should never happen
					fast_send_resp(mut conn, vweb.http_500) or {}
					params.file_responses[fd].done()
					pv.close_conn(fd)
					return
				}
				params.file_responses[fd].open = true

				res := pv.add(fd, picoev.picoev_write, params.timeout_in_seconds, picoev.raw_callback)
				// picoev error
				if res == -1 {
					// should not happen
					fast_send_resp(mut conn, vweb.http_500) or {}
					params.file_responses[fd].done()
					pv.close_conn(fd)
					return
				}
				// no errors we can send the HTTP headers
				fast_send_resp_header(mut conn, completed_context.res) or {}
			}
		}
	} else {
		// invalid request headers/data
		pv.close_conn(fd)
	}
}

// close the connection when `should_close` is true.
@[inline]
fn handle_complete_request(should_close bool, mut pv picoev.Picoev, fd int) {
	if should_close {
		pv.close_conn(fd)
	}
}

fn handle_request[A, X](mut conn net.TcpConn, req http.Request, params &RequestParams) ?&Context {
	mut global_app := unsafe { &A(params.global_app) }

	// TODO: change this variable to include the total wait time over each network cycle
	// maybe store it in Request.user_ptr ?
	page_gen_start := time.ticks()

	$if trace_request ? {
		dump(req)
	}
	$if trace_request_url ? {
		dump(req.url)
	}

	// parse the URL, query and form data
	mut url := urllib.parse(req.url) or {
		eprintln('[vweb] error parsing path "${req.url}": ${err}')
		return none
	}
	query := parse_query_from_url(url)
	form, files := parse_form_from_request(req) or {
		// Bad request
		eprintln('[vweb] error parsing form: ${err.msg()}')
		conn.write(vweb.http_400.bytes()) or {}
		return none
	}

	// remove the port from the HTTP Host header
	host_with_port := req.header.get(.host) or { '' }
	host, _ := urllib.split_host_port(host_with_port)

	// Create Context with request data
	mut ctx := &Context{
		req:            req
		page_gen_start: page_gen_start
		conn:           conn
		query:          query
		form:           form
		files:          files
	}

	if connection_header := req.header.get(.connection) {
		// A client that does not support persistent connections MUST send the
		// "close" connection option in every request message.
		if connection_header.to_lower() == 'close' {
			ctx.client_wants_to_close = true
		}
	}

	$if A is StaticApp {
		ctx.custom_mime_types = global_app.static_mime_types.clone()
	}

	// match controller paths
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers, ctx, mut url,
			host)
		{
			return completed_context
		}
	}

	// create a new user context and pass the vweb's context
	mut user_context := X{}
	user_context.Context = ctx

	handle_route[A, X](mut global_app, mut user_context, url, host, params.routes)
	// we need to explicitly tell the V compiler to return a reference
	return &user_context.Context
}

fn handle_route[A, X](mut app A, mut user_context X, url urllib.URL, host string, routes &map[string]Route) {
	mut route := Route{}
	mut middleware_has_sent_response := false
	mut not_found := false

	defer {
		// execute middleware functions after vweb is done and before the response is send
		mut was_done := true
		$if A is MiddlewareApp {
			if !not_found && !middleware_has_sent_response {
				// if the middleware doesn't send an alternate response, but only changes the
				// response object we only have to check if the `done` was previously set to true
				was_done = user_context.Context.done
				// reset `done` so the middleware functions can return a different response
				// 1 time only, since the `done` guard is still present in
				// `Context.send_response_to_client`
				user_context.Context.done = false

				// no need to check the result of `validate_middleware`, since a response has to be sent
				// anyhow. This function makes sure no further middleware is executed.
				validate_middleware[X](mut user_context, app.Middleware.get_global_handlers_after[X]())
				validate_middleware[X](mut user_context, route.after_middlewares)
			}
		}
		// send only the headers, because if the response body is too big, TcpConn code will
		// actually block, because it has to wait for the socket to become ready to write. Vweb
		// will handle this case.
		if !was_done && !user_context.Context.done && !user_context.Context.takeover {
			eprintln('[vweb] handler for route "${url.path}" does not send any data!')
			// send response anyway so the connection won't block
			// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
		} else if !user_context.Context.takeover {
			// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
		}
		// Context.takeover is set to true, so the user must close the connection and sent a response.
	}

	url_words := url.path.split('/').filter(it != '')

	$if vweb_livereload ? {
		if url.path.starts_with('/vweb_livereload/') {
			if url.path.ends_with('current') {
				user_context.handle_vweb_livereload_current()
				return
			}
			if url.path.ends_with('script.js') {
				user_context.handle_vweb_livereload_script()
				return
			}
		}
	}

	// first execute before_request
	$if A is HasBeforeRequest {
		app.before_request()
	}
	// user_context.before_request()
	if user_context.Context.done {
		return
	}

	// then execute global middleware functions
	$if A is MiddlewareApp {
		if validate_middleware[X](mut user_context, app.Middleware.get_global_handlers[X]()) == false {
			middleware_has_sent_response = true
			return
		}
	}

	$if A is StaticApp {
		if serve_if_static[A, X](app, mut user_context, url, host) {
			// successfully served a static file
			return
		}
	}

	// Route matching and match route specific middleware as last step
	$for method in A.methods {
		$if method.return_type is Result {
			route = (*routes)[method.name] or {
				eprintln('[vweb] parsed attributes for the `${method.name}` are not found, skipping...')
				Route{}
			}

			// Skip if the HTTP request method does not match the attributes
			if user_context.Context.req.method in route.methods {
				// Used for route matching
				route_words := route.path.split('/').filter(it != '')

				// Skip if the host does not match or is empty
				if route.host == '' || route.host == host {
					can_have_data_args := user_context.Context.req.method == .post
						|| user_context.Context.req.method == .get
					// Route immediate matches first
					// For example URL `/register` matches route `/:user`, but `fn register()`
					// should be called first.
					if !route.path.contains('/:') && url_words == route_words {
						// We found a match
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, route.middlewares) == false {
								middleware_has_sent_response = true
								return
							}
						}

						if method.args.len > 1 && can_have_data_args {
							// Populate method args with form or query values
							mut args := []string{cap: method.args.len + 1}
							data := if user_context.Context.req.method == .get {
								user_context.Context.query
							} else {
								user_context.Context.form
							}

							for param in method.args[1..] {
								args << data[param.name]
							}

							app.$method(mut user_context, args)
						} else {
							app.$method(mut user_context)
						}
						return
					}

					if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, route.middlewares) == false {
								middleware_has_sent_response = true
								return
							}
						}

						if method.args.len > 1 && can_have_data_args {
							// Populate method args with form or query values
							mut args := []string{cap: method.args.len + 1}

							data := if user_context.Context.req.method == .get {
								user_context.Context.query
							} else {
								user_context.Context.form
							}

							for param in method.args[1..] {
								args << data[param.name]
							}

							app.$method(mut user_context, args)
						} else {
							app.$method(mut user_context)
						}
						return
					}

					if params := route_matches(url_words, route_words) {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, route.middlewares) == false {
								middleware_has_sent_response = true
								return
							}
						}

						method_args := params.clone()
						if method_args.len + 1 != method.args.len {
							eprintln('[vweb] warning: uneven parameters count (${method.args.len}) in `${method.name}`, compared to the vweb route `${method.attrs}` (${method_args.len})')
						}
						app.$method(mut user_context, method_args)
						return
					}
				}
			}
		}
	}
	// return 404
	user_context.not_found()
	not_found = true
	return
}

fn route_matches(url_words []string, route_words []string) ?[]string {
	// URL path should be at least as long as the route path
	// except for the catchall route (`/:path...`)
	if route_words.len == 1 && route_words[0].starts_with(':') && route_words[0].ends_with('...') {
		return ['/' + url_words.join('/')]
	}
	if url_words.len < route_words.len {
		return none
	}

	mut params := []string{cap: url_words.len}
	if url_words.len == route_words.len {
		for i in 0 .. url_words.len {
			if route_words[i].starts_with(':') {
				// We found a path parameter
				params << url_words[i]
			} else if route_words[i] != url_words[i] {
				// This url does not match the route
				return none
			}
		}
		return params
	}

	// The last route can end with ... indicating an array
	if route_words.len == 0 || !route_words[route_words.len - 1].ends_with('...') {
		return none
	}

	for i in 0 .. route_words.len - 1 {
		if route_words[i].starts_with(':') {
			// We found a path parameter
			params << url_words[i]
		} else if route_words[i] != url_words[i] {
			// This url does not match the route
			return none
		}
	}
	params << url_words[route_words.len - 1..url_words.len].join('/')
	return params
}

// check if request is for a static file and serves it
// returns true if we served a static file, false otherwise
@[manualfree]
fn serve_if_static[A, X](app &A, mut user_context X, url urllib.URL, host string) bool {
	// TODO: handle url parameters properly - for now, ignore them
	mut asked_path := url.path
	base_path := os.base(asked_path)

	if !base_path.contains('.') && !asked_path.ends_with('/') {
		asked_path += '/'
	}

	if asked_path.ends_with('/') {
		if app.static_files[asked_path + 'index.html'] != '' {
			asked_path += 'index.html'
		} else if app.static_files[asked_path + 'index.htm'] != '' {
			asked_path += 'index.htm'
		}
	}
	static_file := app.static_files[asked_path] or { return false }

	// StaticHandler ensures that the mime type exists on either the App or in vweb
	ext := os.file_ext(static_file)
	mut mime_type := app.static_mime_types[ext] or { vweb.mime_types[ext] }

	static_host := app.static_hosts[asked_path] or { '' }
	if static_file == '' || mime_type == '' {
		return false
	}
	if static_host != '' && static_host != host {
		return false
	}

	user_context.send_file(mime_type, static_file)
	return true
}

// send a string over `conn`
fn send_string(mut conn net.TcpConn, s string) ! {
	$if trace_send_string_conn ? {
		eprintln('> send_string: conn: ${ptr_str(conn)}')
	}
	$if trace_response ? {
		eprintln('> send_string:\n${s}\n')
	}
	if voidptr(conn) == unsafe { nil } {
		return error('connection was closed before send_string')
	}
	conn.write_string(s)!
}

// send a string ptr over `conn`
fn send_string_ptr(mut conn net.TcpConn, ptr &u8, len int) !int {
	$if trace_send_string_conn ? {
		eprintln('> send_string: conn: ${ptr_str(conn)}')
	}
	// $if trace_response ? {
	// 	eprintln('> send_string:\n${s}\n')
	// }
	if voidptr(conn) == unsafe { nil } {
		return error('connection was closed before send_string')
	}
	return conn.write_ptr(ptr, len)
}

fn fast_send_resp_header(mut conn net.TcpConn, resp http.Response) ! {
	mut sb := strings.new_builder(resp.body.len + 200)
	sb.write_string('HTTP/')
	sb.write_string(resp.http_version)
	sb.write_string(' ')
	sb.write_decimal(resp.status_code)
	sb.write_string(' ')
	sb.write_string(resp.status_msg)
	sb.write_string('\r\n')

	resp.header.render_into_sb(mut sb,
		version: resp.version()
	)
	sb.write_string('\r\n')
	send_string(mut conn, sb.str())!
}

// Formats resp to a string suitable for HTTP response transmission
// A fast version of `resp.bytestr()` used with
// `send_string(mut ctx.conn, resp.bytestr())`
fn fast_send_resp(mut conn net.TcpConn, resp http.Response) ! {
	fast_send_resp_header(mut conn, resp)!
	send_string(mut conn, resp.body)!
}

// Set s to the form error
pub fn (mut ctx Context) error(s string) {
	eprintln('[vweb] Context.error: ${s}')
	ctx.form_error = s
}
