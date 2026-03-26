module v3

// Client lifecycle operations: graceful shutdown and request cancellation.

// cancel_request cancels an in-flight HTTP/3 request by resetting its QUIC
// stream with H3_REQUEST_CANCELLED (RFC 9114 §4.1.1). The peer will receive
// a RESET_STREAM frame and should discard any partial response.
pub fn (mut c Client) cancel_request(stream_id u64) ! {
	c.quic_conn.reset_stream(stream_id, u64(H3ErrorCode.h3_request_cancelled))!
}

// send_goaway sends a GOAWAY frame on the control stream. The stream_id
// indicates the highest stream ID that might have been processed. Peers
// should use H3 error codes from H3ErrorCode when closing the connection.
pub fn (mut c Client) send_goaway(stream_id u64) ! {
	if c.uni.control_stream_id < 0 {
		return error('control stream not opened')
	}
	ctrl_id := u64(c.uni.control_stream_id)

	payload := encode_varint(stream_id)!

	mut data := []u8{}
	data << encode_varint(u64(FrameType.goaway))!
	data << encode_varint(u64(payload.len))!
	data << payload

	c.quic_conn.send(ctrl_id, data)!
}
