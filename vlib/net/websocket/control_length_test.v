module websocket

// test_send_control_frame_rejects_oversize_payload checks that a control
// frame payload longer than the RFC 6455 section 5.5 limit of 125 bytes is
// rejected with an error. Without the check the length byte truncates to 7
// bits, so the peer reads a short payload and treats the remainder as the
// next frame, desynchronising its parser.
// See https://github.com/vlang/v/issues/30175
fn test_send_control_frame_rejects_oversize_payload() {
	mut ws := Client{
		is_server: true
	}
	ws.set_state(.open)
	big := []u8{len: 200}
	if _ := ws.send_control_frame(.ping, 'PING', big) {
		assert false, 'control frame payload over 125 bytes must be rejected'
	} else {
		assert err.msg() == 'control frame payload exceeds 125 bytes'
	}
}
