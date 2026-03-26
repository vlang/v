module v2

// Frame dispatch loop and request handling for HTTP/2 server connections.

fn (mut s Server) run_frame_loop(mut conn ServerConn, mut ctx ConnContext) u32 {
	mut decoder := new_decoder()
	mut client_settings := ClientSettings{}
	mut streams := map[u32]ServerStreamState{}
	mut highest_stream_id := u32(0)
	mut conn_bytes_received := u32(0)

	for {
		frame := s.read_frame(mut conn) or {
			if err.msg().contains('EOF') {
				break
			}
			eprintln('[HTTP/2] Read frame error: ${err}')
			break
		}

		match frame.header.frame_type {
			.settings {
				s.handle_settings(mut conn, frame, mut client_settings, mut ctx) or {
					eprintln('[HTTP/2] Settings error: ${err}')
				}
			}
			.headers {
				stream_id := frame.header.stream_id
				if stream_id > 0 && stream_id > highest_stream_id {
					highest_stream_id = stream_id
				}
				s.handle_headers_in_loop(frame, mut streams, mut ctx, mut conn, mut decoder,
					client_settings)
			}
			.data {
				conn_bytes_received = s.handle_data_in_loop(frame, mut streams, mut ctx, mut
					conn, conn_bytes_received)
			}
			.priority {
				handle_priority(frame)
			}
			.ping {
				s.handle_ping(mut conn, frame) or { eprintln('[HTTP/2] Ping error: ${err}') }
			}
			.window_update {
				handle_window_update_in_loop(frame, mut ctx, mut conn) or {
					eprintln('[HTTP/2] WINDOW_UPDATE connection error: ${err}')
					break
				}
			}
			.rst_stream {
				ctx.flow.remove_stream(frame.header.stream_id)
				streams.delete(frame.header.stream_id)
			}
			else {}
		}
	}

	return highest_stream_id
}

fn (mut s Server) handle_headers_in_loop(frame Frame, mut streams map[u32]ServerStreamState, mut ctx ConnContext, mut conn ServerConn, mut decoder Decoder, cs ClientSettings) {
	stream_id := frame.header.stream_id
	if stream_id == 0 {
		eprintln('[HTTP/2] HEADERS on stream 0 (protocol error)')
		return
	}
	if streams.len >= int(s.config.max_concurrent_streams) {
		send_rst_stream(mut conn, stream_id, .refused_stream) or {}
		return
	}
	decoded := decoder.decode(frame.payload) or {
		eprintln('[HTTP/2] Header decode error: ${err}')
		return
	}
	mut method := ''
	mut path := ''
	mut header_map := map[string]string{}
	for h in decoded {
		match h.name {
			':method' { method = h.value }
			':path' { path = h.value }
			else { header_map[h.name] = h.value }
		}
	}
	streams[stream_id] = ServerStreamState{
		method:     method
		path:       path
		header_map: header_map.clone()
	}
	ctx.flow.init_stream(stream_id, cs.initial_window_size)
	if frame.header.has_flag(.end_stream) {
		stream := streams[stream_id]
		streams.delete(stream_id)
		ctx.flow.remove_stream(stream_id)
		request := build_request(stream_id, stream)
		ctx.wg.add(1)
		spawn s.dispatch_stream(mut conn, request, mut ctx)
	}
}

fn (mut s Server) handle_data_in_loop(frame Frame, mut streams map[u32]ServerStreamState, mut ctx ConnContext, mut conn ServerConn, conn_bytes_received u32) u32 {
	stream_id := frame.header.stream_id
	if stream_id !in streams {
		send_rst_stream(mut conn, stream_id, .protocol_error) or {}
		return conn_bytes_received
	}
	df := DataFrame.from_frame(frame) or {
		eprintln('[HTTP/2] DATA parse error: ${err}')
		return conn_bytes_received
	}
	data_len := u32(df.data.len)
	streams[stream_id].body << df.data

	mut updated_bytes := conn_bytes_received + data_len
	conn_wu_threshold := s.config.initial_window_size / 2
	if conn_wu_threshold > 0 && updated_bytes >= conn_wu_threshold {
		send_window_update(mut conn, 0, updated_bytes) or {
			eprintln('[HTTP/2] Failed to send connection WINDOW_UPDATE: ${err}')
		}
		updated_bytes = 0
	}
	if data_len > 0 {
		send_window_update(mut conn, stream_id, data_len) or {
			eprintln('[HTTP/2] Failed to send stream WINDOW_UPDATE: ${err}')
		}
	}
	if df.end_stream {
		stream := streams[stream_id]
		streams.delete(stream_id)
		request := build_request(stream_id, stream)
		ctx.wg.add(1)
		spawn s.dispatch_stream(mut conn, request, mut ctx)
	}
	return updated_bytes
}

fn handle_window_update_in_loop(frame Frame, mut ctx ConnContext, mut conn ServerConn) ! {
	wuf := WindowUpdateFrame.from_frame(frame) or {
		eprintln('[HTTP/2] Invalid WINDOW_UPDATE: ${err}')
		return
	}
	if frame.header.stream_id == 0 {
		ctx.flow.update_connection_window(wuf.window_increment)!
	} else {
		ctx.flow.update_stream_window(frame.header.stream_id, wuf.window_increment) or {
			send_rst_stream(mut conn, frame.header.stream_id, .protocol_error) or {}
			return
		}
	}
}

fn build_request(stream_id u32, stream ServerStreamState) ServerRequest {
	return ServerRequest{
		method:    stream.method
		path:      stream.path
		headers:   stream.header_map
		body:      stream.body
		stream_id: stream_id
	}
}

fn (mut s Server) dispatch_stream(mut conn ServerConn, request ServerRequest, mut ctx ConnContext) {
	defer {
		ctx.flow.remove_stream(request.stream_id)
		ctx.wg.done()
	}

	h := s.handler or {
		ctx.write_mu.lock()
		error_response := ServerResponse{
			status_code: 500
			headers:     {
				'content-type': 'text/plain'
			}
			body:        'no handler configured'.bytes()
		}
		s.send_response(mut conn, request.stream_id, error_response, mut ctx.encoder, mut
			ctx.flow) or { eprintln('[HTTP/2] Failed to send error response: ${err}') }
		ctx.write_mu.unlock()
		return
	}

	response := h(request)

	ctx.write_mu.lock()
	s.send_response(mut conn, request.stream_id, response, mut ctx.encoder, mut ctx.flow) or {
		eprintln('[HTTP/2] Failed to send response: ${err}')
	}
	ctx.write_mu.unlock()
}
