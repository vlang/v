module v2

// Frame dispatch loop and request handling for HTTP/2 server connections.

// LoopState holds mutable state for the server frame processing loop.
struct LoopState {
mut:
	decoder             Decoder
	client_settings     ClientSettings
	streams             map[u32]ServerStreamState
	highest_stream_id   u32
	conn_bytes_received u32
	continuation_state  ContinuationState
}

fn (mut s Server) run_frame_loop(mut conn ServerConn, mut ctx ConnContext) u32 {
	mut state := LoopState{
		decoder: new_decoder()
	}

	for {
		frame := s.read_frame(mut conn) or {
			if err.msg().contains('EOF') {
				break
			}
			send_goaway_and_close(mut conn, state.highest_stream_id, .protocol_error,
				'read frame error: ${err}') or {}
			break
		}

		s.dispatch_frame(frame, mut conn, mut ctx, mut state) or { break }
	}

	return state.highest_stream_id
}

// dispatch_frame routes a single frame to the appropriate handler.
fn (mut s Server) dispatch_frame(frame Frame, mut conn ServerConn, mut ctx ConnContext, mut state LoopState) ! {
	match frame.header.frame_type {
		.settings {
			s.handle_settings(mut conn, frame, mut state.client_settings, mut ctx) or {
				return send_goaway_and_close(mut conn, state.highest_stream_id, .protocol_error,
					'settings error: ${err}')
			}
		}
		.headers {
			sid := frame.header.stream_id
			if sid > 0 && sid > state.highest_stream_id {
				state.highest_stream_id = sid
			}
			s.handle_headers_in_loop(frame, mut state.streams, mut ctx, mut conn, mut
				state.decoder, state.client_settings, mut state.continuation_state)
		}
		.data {
			state.conn_bytes_received = s.handle_data_in_loop(frame, mut state.streams, mut
				ctx, mut conn, state.conn_bytes_received)
		}
		.continuation {
			handle_continuation_in_loop(frame, mut state.continuation_state, mut state.streams, mut
				conn, mut ctx, mut state.decoder, state.highest_stream_id, state.client_settings, mut
				s)!
		}
		.priority {
			handle_priority(frame)
		}
		.ping {
			s.handle_ping(mut conn, frame) or {
				return send_goaway_and_close(mut conn, state.highest_stream_id, .protocol_error,
					'ping error: ${err}')
			}
		}
		.window_update {
			handle_window_update_in_loop(frame, mut ctx, mut conn) or {
				return send_goaway_and_close(mut conn, state.highest_stream_id, .protocol_error,
					'window_update error: ${err}')
			}
		}
		.rst_stream {
			ctx.flow.remove_stream(frame.header.stream_id)
			state.streams.delete(frame.header.stream_id)
		}
		else {
			return send_goaway_and_close(mut conn, state.highest_stream_id, .protocol_error,
				'unsupported frame type')
		}
	}
}

fn (mut s Server) handle_headers_in_loop(frame Frame, mut streams map[u32]ServerStreamState, mut ctx ConnContext, mut conn ServerConn, mut decoder Decoder, cs ClientSettings, mut cont ContinuationState) {
	stream_id := frame.header.stream_id
	if stream_id == 0 {
		eprintln('[HTTP/2] HEADERS on stream 0 (protocol error)')
		return
	}
	if streams.len >= int(s.config.max_concurrent_streams) {
		send_rst_stream(mut conn, stream_id, .refused_stream) or {}
		return
	}
	hf := HeadersFrame.from_frame(frame) or {
		send_goaway_and_close(mut conn, stream_id, .protocol_error, 'invalid HEADERS frame: ${err}') or {}
		return
	}
	if !hf.end_headers {
		cont.stream_id = stream_id
		cont.raw_header_block = hf.headers.clone()
		cont.count = 0
		return
	}
	decoded := decoder.decode(hf.headers) or {
		eprintln('[HTTP/2] Header decode error: ${err}')
		return
	}
	method, path, header_map := extract_pseudo_headers(decoded)
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

fn extract_pseudo_headers(decoded []HeaderField) (string, string, map[string]string) {
	mut method_str := ''
	mut path := ''
	mut headers := map[string]string{}
	for h in decoded {
		match h.name {
			':method' { method_str = h.value }
			':path' { path = h.value }
			else { headers[h.name] = h.value }
		}
	}
	return method_str, path, headers
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

// ContinuationState tracks server-side CONTINUATION frame accumulation per stream.
struct ContinuationState {
mut:
	stream_id        u32
	raw_header_block []u8
	count            int
}

fn send_goaway_and_close(mut conn ServerConn, last_stream_id u32, error_code ErrorCode, debug_msg string) ! {
	goaway := GoAwayFrame{
		last_stream_id: last_stream_id
		error_code:     error_code
		debug_data:     debug_msg.bytes()
	}
	frame_bytes := goaway.to_frame().encode()
	conn.write(frame_bytes) or {}
	return error('GOAWAY sent: ${debug_msg}')
}

fn handle_continuation_in_loop(frame Frame, mut cont ContinuationState, mut streams map[u32]ServerStreamState, mut conn ServerConn, mut ctx ConnContext, mut decoder Decoder, highest_stream_id u32, cs ClientSettings, mut s Server) ! {
	stream_id := frame.header.stream_id
	if stream_id == 0 {
		return send_goaway_and_close(mut conn, highest_stream_id, .protocol_error, 'CONTINUATION on stream 0')
	}
	cont.count++
	if cont.count > max_continuation_frames {
		cont = ContinuationState{}
		return send_goaway_and_close(mut conn, highest_stream_id, .enhance_your_calm,
			'CONTINUATION flood')
	}
	if cont.raw_header_block.len + frame.payload.len > max_header_block_size {
		cont = ContinuationState{}
		return send_goaway_and_close(mut conn, highest_stream_id, .enhance_your_calm,
			'header block too large')
	}
	cont.stream_id = stream_id
	cont.raw_header_block << frame.payload
	if !frame.header.has_flag(.end_headers) {
		return
	}
	decoded := decoder.decode(cont.raw_header_block) or {
		cont = ContinuationState{}
		send_rst_stream(mut conn, stream_id, .compression_error) or {}
		return
	}
	cont = ContinuationState{}
	apply_decoded_headers(decoded, stream_id, mut streams, mut ctx, mut conn, cs, mut
		s)
}

fn apply_decoded_headers(decoded []HeaderField, stream_id u32, mut streams map[u32]ServerStreamState, mut ctx ConnContext, mut conn ServerConn, cs ClientSettings, mut s Server) {
	method, path, header_map := extract_pseudo_headers(decoded)
	streams[stream_id] = ServerStreamState{
		method:     method
		path:       path
		header_map: header_map.clone()
	}
	ctx.flow.init_stream(stream_id, cs.initial_window_size)
}
