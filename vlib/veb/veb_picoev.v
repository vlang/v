module veb

import os

$if !new_veb ? {
	import picoev
	import time
	import net
	import net.http
	import io
	import net.urllib
}

$if !new_veb ? {
	// run_at - start a new veb server, listening only on a specific address `host`, at the specified `port`
	// Usage example: veb.run_at(new_app(), host: 'localhost' port: 8099 family: .ip)!
	@[direct_array_access; manualfree]
	pub fn run_at[A, X](mut global_app A, params RunParams) ! {
		if params.port <= 0 || params.port > 65535 {
			return error('invalid port number `${params.port}`, it should be between 1 and 65535')
		}

		routes := generate_routes[A, X](global_app)!
		controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!

		if params.show_startup_message {
			host := if params.host == '' { 'localhost' } else { params.host }
			println('[veb] Running app on http://${host}:${params.port}/')
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
		pico_context.buf = unsafe { malloc_noscan(picoev.max_fds * max_read + 1) }
		defer {
			unsafe {
				free(pico_context.buf)
			}
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
					eprintln('[veb] error: write event on connection should be closed')
				}
				pv.close_conn(fd)
			}
		} else if events == picoev.picoev_read {
			$if trace_picoev_callback ? {
				eprintln('> read event on file descriptor ${fd}')
			}
			// println('ev_callback fd=${fd} params.routes=${params.routes.len}')
			handle_read[A, X](mut pv, mut params, fd)
		} else {
			// should never happen
			eprintln('[veb] error: invalid picoev event ${events}')
		}
	}

	fn handle_timeout(mut pv picoev.Picoev, mut params RequestParams, fd int) {
		mut conn := &net.TcpConn{
			sock:        net.tcp_socket_from_handle_raw(fd)
			handle:      fd
			is_blocking: false
		}

		fast_send_resp(mut conn, http_408) or {}
		pv.close_conn(fd)
		params.request_done(fd)
	}

	// handle_write_file reads data from a file and sends that data over the socket.
	@[direct_array_access; manualfree]
	fn handle_write_file(mut pv picoev.Picoev, mut params RequestParams, fd int) {
		mut bytes_to_write := int(params.file_responses[fd].total - params.file_responses[fd].pos)

		$if linux || freebsd {
			bytes_written := sendfile(fd, params.file_responses[fd].file.fd, bytes_to_write)
			if bytes_written < 0 {
				params.file_responses[fd].pos += bytes_to_write
			} else {
				params.file_responses[fd].pos += bytes_written
			}
		} $else {
			if bytes_to_write > max_write {
				bytes_to_write = max_write
			}
			data := unsafe { malloc(bytes_to_write) }
			defer(fn) {
				unsafe { free(data) }
			}
			mut conn := &net.TcpConn{
				sock:          net.tcp_socket_from_handle_raw(fd)
				handle:        fd
				is_blocking:   false
				write_timeout: params.timeout_in_seconds * time.second
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
			handle_complete_request(params.file_responses[fd].should_close_conn, mut pv,
				fd)
			return
		}
	}

	// handle_write_string reads data from a string and sends that data over the socket
	@[direct_array_access]
	fn handle_write_string(mut pv picoev.Picoev, mut params RequestParams, fd int) {
		mut bytes_to_write := int(params.string_responses[fd].str.len - params.string_responses[fd].pos)
		if bytes_to_write > max_write {
			bytes_to_write = max_write
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
			handle_complete_request(params.string_responses[fd].should_close_conn, mut
				pv, fd)
			return
		}
	}

	// handle_read reads data from the connection and if the request is complete
	// it calls `handle_route` and closes the connection.
	// If the request is not complete, it stores the incomplete request in `params`
	// and the connection stays open until it is ready to read again
	@[direct_array_access; manualfree]
	fn handle_read[A, X](mut pv picoev.Picoev, mut params RequestParams, fd int) {
		// println('handle_read() fd=${fd} params.routes=${params.routes}')
		mut conn := &net.TcpConn{
			sock:        net.tcp_socket_from_handle_raw(fd)
			handle:      fd
			is_blocking: false
		}

		// cap the max_read to 8KB
		mut reader := io.new_buffered_reader(reader: conn, cap: max_read)
		defer {
			unsafe {
				reader.free()
			}
		}
		// take the previous incomplete request
		mut req := params.incomplete_requests[fd]
		// check if there is an incomplete request for this file descriptor
		if params.idx[fd] == 0 {
			$if trace_handle_read ? {
				eprintln('>>>>> fd: ${fd} | start of request parsing')
			}
			// this is the start of a new request, setup the connection, and read the headers:
			// set the read and write timeout according to picoev settings when the
			// connection is first encountered
			conn.set_read_timeout(params.timeout_in_seconds)
			conn.set_write_timeout(params.timeout_in_seconds)
			// first time that this connection is being read from, so we parse the
			// request header first
			req = http.parse_request_head(mut reader) or {
				// Prevents errors from being thrown when BufferedReader is empty
				if err !is io.Eof {
					eprintln('[veb] error parsing request: ${err}')
				}
				// the buffered reader was empty meaning that the client probably
				// closed the connection.
				pv.close_conn(fd)
				params.request_done(fd)
				return
			}
			if reader.total_read >= max_read {
				// throw an error when the request header is larger than 8KB
				// same limit that apache handles
				eprintln('[veb] error parsing request: too large')
				fast_send_resp(mut conn, http_413) or {}
				pv.close_conn(fd)
				params.request_done(fd)
				return
			}
		}
		if params.idx[fd] == -1 {
			// this is for sure a continuation of a previous request, where the first part contained only headers;
			// make sure that we are ready to accept the body and account for every byte in it, by setting the counter to 0:
			params.idx[fd] = 0
			$if trace_handle_read ? {
				eprintln('>>>>> fd: ${fd} | continuation of request, where the first part contained headers')
			}
		}
		// check if the request has a body
		content_length := req.header.get(.content_length) or { '0' }
		content_length_i := content_length.int()
		if content_length_i > 0 {
			mut max_bytes_to_read := max_read - reader.total_read
			mut bytes_to_read := content_length_i - params.idx[fd]
			// cap the bytes to read to 8KB for the body, including the request headers if any
			if bytes_to_read > max_read - reader.total_read {
				bytes_to_read = max_read - reader.total_read
			}
			mut buf_ptr := params.buf
			unsafe {
				buf_ptr += fd * max_read // pointer magic
			}
			// convert to []u8 for BufferedReader
			mut buf := unsafe { buf_ptr.vbytes(max_bytes_to_read) }
			n := reader.read(mut buf) or {
				if reader.total_read > 0 {
					// The headers were parsed in this cycle, but the body has not been sent yet. No need to error.
					params.idx[fd] = -1 // avoid reparsing the headers on the next call.
					params.incomplete_requests[fd] = req
					$if trace_handle_read ? {
						eprintln('>>>>> fd: ${fd} | request headers were parsed, but the body has not been parsed yet | params.idx[fd]: ${params.idx[fd]} | content_length_i: ${content_length_i}')
					}
					return
				}
				eprintln('[veb] error reading request body: ${err}')
				if err is io.Eof {
					// we expect more data to be send, but an Eof error occurred, meaning
					// that there is no more data to be read from the socket.
					// And at this point we expect that there is data to be read for the body.
					fast_send_resp(mut conn, http.new_response(
						status: .bad_request
						body:   'Mismatch of body length and Content-Length header'
						header: http.new_header(
							key:   .content_type
							value: 'text/plain'
						).join(headers_close)
					)) or {}
				}
				pv.close_conn(fd)
				params.request_done(fd)
				return
			}
			// there is no more data to be sent, but it is less than the Content-Length header
			// so it is a mismatch of body length and content length.
			// Or if there is more data received then the Content-Length header specified
			if (n == 0 && params.idx[fd] != 0) || params.idx[fd] + n > content_length_i {
				fast_send_resp(mut conn, http.new_response(
					status: .bad_request
					body:   'Mismatch of body length and Content-Length header'
					header: http.new_header(
						key:   .content_type
						value: 'text/plain'
					).join(headers_close)
				)) or {}
				pv.close_conn(fd)
				params.request_done(fd)
				return
			} else if n < bytes_to_read || params.idx[fd] + n < content_length_i {
				// request is incomplete wait until the socket becomes ready to read again
				// TODO: change this to a memcpy function?
				req.data += buf[0..n].bytestr()
				params.incomplete_requests[fd] = req
				params.idx[fd] += n
				$if trace_handle_read ? {
					eprintln('>>>>> request is NOT complete, fd: ${fd} | n: ${n} | req.data.len: ${req.data.len} | params.idx[fd]: ${params.idx[fd]}')
				}
				return
			} else {
				// request is complete: n = bytes_to_read
				req.data += buf[0..n].bytestr()
				params.idx[fd] += n
				$if trace_handle_read ? {
					eprintln('>>>>> request is NOW COMPLETE, fd: ${fd} | n: ${n} | req.data.len: ${req.data.len}')
				}
			}
		}
		defer {
			params.request_done(fd)
		}
		if completed_context := handle_request[A, X](mut conn, req, params) {
			if completed_context.takeover {
				// the connection should be kept open, but removed from the picoev loop.
				// This way veb can continue handling other connections and the user can
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
					if completed_context.res.body.len < max_read {
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
							fast_send_resp(mut conn, http_500) or {}
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
						fast_send_resp(mut conn, http_500) or {}
						return
					}
					params.file_responses[fd].total = length.i64()
					params.file_responses[fd].file = os.open(completed_context.return_file) or {
						// Context checks if the file is valid, so this should never happen
						fast_send_resp(mut conn, http_500) or {}
						params.file_responses[fd].done()
						pv.close_conn(fd)
						return
					}
					params.file_responses[fd].open = true

					res := pv.add(fd, picoev.picoev_write, params.timeout_in_seconds,
						picoev.raw_callback)
					// picoev error
					if res == -1 {
						// should not happen
						fast_send_resp(mut conn, http_500) or {}
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
		// println('handle_request() params.routes=${params.routes}')
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
			eprintln('[veb] error parsing path "${req.url}": ${err}')
			return none
		}
		query := parse_query_from_url(url)
		form, files := parse_form_from_request(req) or {
			// Bad request
			eprintln('[veb] error parsing form: ${err.msg()}')
			conn.write(http_400.bytes()) or {}
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
			if completed_context := handle_controllers[X](params.controllers, ctx, mut
				url, host)
			{
				return completed_context
			}
		}

		// create a new user context and pass the veb's context
		mut user_context := X{}
		user_context.Context = ctx

		handle_route[A, X](mut global_app, mut user_context, url, host, params.routes)
		// we need to explicitly tell the V compiler to return a reference
		return &user_context.Context
	}
}
