module grpc

// gRPC message framing: a 5-byte prefix (1-byte compressed flag, 4-byte
// big-endian length) before each protobuf payload, on requests and responses.

pub const frame_header_len = 5

// max_frame_len rejects absurd lengths before allocating; gRPC's own default
// receive limit is 4 MiB, we allow 32.
pub const max_frame_len = u32(32 * 1024 * 1024)

pub struct Frame {
pub:
	compressed bool
	payload    []u8
}

// encode_frame prefixes payload with the 5-byte header.
pub fn encode_frame(payload []u8, compressed bool) []u8 {
	mut out := []u8{cap: frame_header_len + payload.len}
	out << u8(if compressed { 1 } else { 0 })
	n := u32(payload.len)
	out << u8(n >> 24)
	out << u8(n >> 16)
	out << u8(n >> 8)
	out << u8(n)
	out << payload
	return out
}

// decode_frame reads one frame from the start of data, returning it and the
// bytes consumed. Zero consumed means data is a valid but incomplete prefix
// of a frame — read more and retry.
pub fn decode_frame(data []u8) !(Frame, int) {
	if data.len < frame_header_len {
		return Frame{}, 0
	}
	flag := data[0]
	if flag > 1 {
		return error('grpc: invalid compressed flag ${flag}')
	}
	n := u32(data[1]) << 24 | u32(data[2]) << 16 | u32(data[3]) << 8 | u32(data[4])
	if n > max_frame_len {
		return error('grpc: frame length ${n} exceeds limit ${max_frame_len}')
	}
	total := frame_header_len + int(n)
	if data.len < total {
		return Frame{}, 0
	}
	return Frame{
		compressed: flag == 1
		payload:    data[frame_header_len..total].clone()
	}, total
}

// decode_frames reads a complete stream of frames, erroring on trailing bytes.
pub fn decode_frames(data []u8) ![]Frame {
	mut frames := []Frame{}
	mut off := 0
	for off < data.len {
		frame, n := decode_frame(data[off..])!
		if n == 0 {
			return error('grpc: truncated frame at offset ${off}')
		}
		frames << frame
		off += n
	}
	return frames
}
