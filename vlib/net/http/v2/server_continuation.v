module v2

// CONTINUATION frame accumulation and header application for HTTP/2 server.

// ContinuationState tracks server-side CONTINUATION frame accumulation per stream.
struct ContinuationState {
mut:
	stream_id        u32
	raw_header_block []u8
	count            int
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

fn apply_decoded_headers(raw_decoded []HeaderField, stream_id u32, mut streams map[u32]ServerStreamState, mut ctx ConnContext, mut conn ServerConn, cs ClientSettings, mut s Server) {
	decoded := join_cookie_headers(raw_decoded)
	validate_request_headers(decoded) or {
		$if trace_http2 ? {
			eprintln('[HTTP/2] Malformed request on stream ${stream_id}: ${err}')
		}
		send_rst_stream(mut conn, stream_id, .protocol_error) or {}
		return
	}
	method, path, header_map := extract_pseudo_headers(decoded)
	streams[stream_id] = ServerStreamState{
		method:     method
		path:       path
		header_map: header_map.clone()
	}
	ctx.flow.init_stream(stream_id, cs.initial_window_size)
}
