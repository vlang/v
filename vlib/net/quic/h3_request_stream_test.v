module quic

fn test_h3_request_stream_starts_awaiting_response_headers() {
	s := new_h3_request_stream_state()
	assert s.phase() == .awaiting_response_headers
}

fn test_h3_request_stream_rejects_non_headers_first_frame() {
	mut s := new_h3_request_stream_state()
	if _ := s.note_frame_kind(false, true) {
		assert false, 'a DATA frame first must be rejected'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
	assert s.phase() == .awaiting_response_headers
}

fn test_h3_request_stream_accepts_headers_then_data_then_trailers() {
	mut s := new_h3_request_stream_state()
	s.note_frame_kind(true, false)! // response headers
	assert s.phase() == .in_body
	s.note_frame_kind(false, true)! // DATA
	assert s.phase() == .in_body
	s.note_frame_kind(false, true)! // more DATA
	assert s.phase() == .in_body
	s.note_frame_kind(true, false)! // trailers
	assert s.phase() == .trailers_received
}

fn test_h3_request_stream_rejects_non_data_non_headers_mid_body() {
	mut s := new_h3_request_stream_state()
	s.note_frame_kind(true, false)!
	// A frame that is neither DATA nor HEADERS (e.g. a decoded PUSH_PROMISE,
	// represented here as frame_is_headers=false, frame_is_data=false).
	if _ := s.note_frame_kind(false, false) {
		assert false, 'a non-DATA, non-HEADERS frame mid-body must be rejected'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
}

fn test_h3_request_stream_rejects_any_frame_after_trailers() {
	mut s := new_h3_request_stream_state()
	s.note_frame_kind(true, false)!
	s.note_frame_kind(true, false)! // trailers
	assert s.phase() == .trailers_received
	if _ := s.note_frame_kind(false, true) {
		assert false, 'no frame is legal after trailers'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
	if _ := s.note_frame_kind(true, false) {
		assert false, 'a second trailers block is not legal either'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
}

fn test_h3_request_stream_note_fin_reaches_done_and_rejects_further_frames() {
	mut s := new_h3_request_stream_state()
	s.note_frame_kind(true, false)!
	s.note_fin()
	assert s.phase() == .done
	if _ := s.note_frame_kind(false, true) {
		assert false, 'no frame is legal once done'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
	// Idempotent.
	s.note_fin()
	assert s.phase() == .done
}

fn test_h3_request_stream_note_fin_directly_from_in_body_skips_trailers() {
	// A response with no trailers: HEADERS -> DATA* -> FIN, no second
	// HEADERS block at all -- perfectly legal per §4.1 ("optionally, one
	// HEADERS frame containing the trailer section, if present").
	mut s := new_h3_request_stream_state()
	s.note_frame_kind(true, false)!
	s.note_frame_kind(false, true)!
	s.note_fin()
	assert s.phase() == .done
}
