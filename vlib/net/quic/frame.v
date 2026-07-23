module quic

// QUIC frame parsing (RFC 9000 §19). Scoped to the frame types usable in
// the Initial and Handshake packet number spaces (RFC 9000 §12.4, Table 3):
// PADDING, PING, ACK, CRYPTO, and CONNECTION_CLOSE (transport-level only --
// the application-level variant, 0x1d, is technically also parseable here
// since it shares CONNECTION_CLOSE's wire shape, but is never legal before
// 1-RTT keys exist). Every other frame type (STREAM, MAX_DATA, ...) is
// deferred to later phases (Phase 6+); parse_frame reports those as "not
// yet implemented", not as a wire-format error, since they are valid QUIC
// frames this module simply doesn't handle yet.

// PaddingFrame represents one or more consecutive PADDING (type 0x00)
// bytes, collapsed into a single frame for convenience. This is purely an
// API choice on the parsing side -- each 0x00 byte remains independently a
// valid, semantically empty PADDING frame on the wire; nothing here changes
// wire compatibility, it only changes how a run of them is reported back to
// the caller.
pub struct PaddingFrame {
pub:
	length int
}

// PingFrame represents a PING (type 0x01) frame: no fields, ack-eliciting.
pub struct PingFrame {}

// AckRange is one reconstructed, already-resolved [smallest, largest]
// inclusive range of acknowledged packet numbers (RFC 9000 §19.3.1) -- the
// wire's Gap/ACK Range Length encoding is resolved into this shape by
// parse_frame so callers never need to re-derive it themselves.
pub struct AckRange {
pub:
	smallest u64
	largest  u64
}

pub struct EcnCounts {
pub:
	ect0   u64
	ect1   u64
	ecn_ce u64
}

// AckFrame represents an ACK frame (type 0x02, or 0x03 when it also
// carries ECN counts). `ranges` is ordered largest-first, matching the wire
// order; ranges are always non-overlapping with at least one unacknowledged
// packet number between consecutive ranges.
//
// `ack_delay` is the RAW wire value (RFC 9000 §19.3): it is NOT yet scaled
// by the peer's `ack_delay_exponent` transport parameter (a connection-level
// value this frame-parsing layer has no access to) -- see
// scaled_ack_delay_micros. It also MUST be ignored entirely for RTT
// sampling purposes in the Initial and Handshake packet number spaces (RFC
// 9002 §5.3); that policy belongs to a later phase's loss-detection code
// (Phase 7), not here -- this struct only carries the raw value forward.
pub struct AckFrame {
pub:
	largest_acknowledged u64
	ack_delay            u64
	ranges               []AckRange
	ecn_counts           ?EcnCounts
}

// default_ack_delay_exponent is RFC 9000 §18.2's default value for the
// `ack_delay_exponent` transport parameter, used when a peer has not yet
// sent (or does not override) it.
pub const default_ack_delay_exponent = u64(3)

// scaled_ack_delay_micros converts an AckFrame's raw `ack_delay` into
// microseconds using the peer's negotiated `ack_delay_exponent` (RFC 9000
// §19.3: `ACK Delay` is the peer's estimate, in ack_delay_exponent-scaled
// units, of the time between receiving the largest-acknowledged packet and
// sending this ACK).
pub fn scaled_ack_delay_micros(raw_ack_delay u64, ack_delay_exponent u64) u64 {
	return raw_ack_delay << ack_delay_exponent
}

// CryptoFrame represents a CRYPTO frame (type 0x06): a chunk of the TLS
// handshake byte stream at one encryption level, positioned at `offset`.
// Reassembling multiple (possibly out-of-order, possibly overlapping)
// CryptoFrames into a contiguous stream is crypto_stream.v's job, not
// this one's -- parse_frame only decodes a single wire frame.
pub struct CryptoFrame {
pub:
	offset u64
	data   []u8
}

// ConnectionCloseFrame represents a CONNECTION_CLOSE frame (type 0x1c
// transport-level, or 0x1d application-level -- `is_application_error`
// distinguishes them). `frame_type` is only meaningful for the
// transport-level variant (the frame type that provoked the close, or 0 if
// unknown/not applicable); the application-level variant has no such field
// on the wire. Mapping these to actual connection-lifecycle behavior
// (closing/draining state, RFC 9000 §10.2) is Phase 8's job
// (connection_close.v) -- this is purely the wire decode.
pub struct ConnectionCloseFrame {
pub:
	is_application_error bool
	error_code           u64
	frame_type           u64
	reason               string
}

pub type QuicFrame = AckFrame | ConnectionCloseFrame | CryptoFrame | PaddingFrame | PingFrame

const frame_type_padding = u64(0x00)
const frame_type_ping = u64(0x01)
const frame_type_ack = u64(0x02)
const frame_type_ack_ecn = u64(0x03)
const frame_type_crypto = u64(0x06)
const frame_type_connection_close_transport = u64(0x1c)
const frame_type_connection_close_application = u64(0x1d)

// parse_frame parses exactly one frame from the start of `buf`, returning
// the frame and the number of bytes consumed. A run of consecutive PADDING
// bytes is consumed as a single PaddingFrame (see its own doc comment).
pub fn parse_frame(buf []u8) !(QuicFrame, int) {
	if buf.len == 0 {
		return error('quic: cannot parse a frame from an empty buffer')
	}
	typ, typ_len := decode_varint(buf)!

	if typ == frame_type_padding {
		mut n := typ_len
		for n < buf.len && buf[n] == 0x00 {
			n++
		}
		return QuicFrame(PaddingFrame{
			length: n
		}), n
	}

	if typ == frame_type_ping {
		return QuicFrame(PingFrame{}), typ_len
	}

	if typ == frame_type_ack || typ == frame_type_ack_ecn {
		return parse_ack_frame(buf, typ_len, typ == frame_type_ack_ecn)
	}

	if typ == frame_type_crypto {
		return parse_crypto_frame(buf, typ_len)
	}

	if typ == frame_type_connection_close_transport
		|| typ == frame_type_connection_close_application {
		return parse_connection_close_frame(buf, typ_len,
			typ == frame_type_connection_close_application)
	}

	return error('quic: frame type 0x${typ:02x} is not yet implemented by this module')
}

fn parse_ack_frame(buf []u8, start int, has_ecn_counts bool) !(QuicFrame, int) {
	mut offset := start
	largest_acknowledged, n1 := decode_varint(buf[offset..])!
	offset += n1
	ack_delay, n2 := decode_varint(buf[offset..])!
	offset += n2
	ack_range_count, n3 := decode_varint(buf[offset..])!
	offset += n3
	first_ack_range, n4 := decode_varint(buf[offset..])!
	offset += n4

	if first_ack_range > largest_acknowledged {
		return error('quic: ACK frame: first_ack_range (${first_ack_range}) exceeds largest_acknowledged (${largest_acknowledged})')
	}

	// ack_range_count is an attacker-controlled varint (up to 2^62-1) --
	// bound it against what the remaining buffer could possibly contain
	// (each range needs at least 2 bytes on the wire: a 1-byte gap varint
	// plus a 1-byte length varint) BEFORE using it to size anything.
	// Skipping this check and passing it straight to a `cap:` allocation
	// hint (as an earlier version of this function did) lets a single
	// small, otherwise-well-formed-looking ACK frame request an
	// enormous upfront allocation -- and narrowing a huge u64 to `int`
	// for that hint is itself unchecked, adding an overflow risk on top
	// of the DoS risk.
	if ack_range_count > u64(buf.len - offset) / 2 {
		return error('quic: ACK frame: ack_range_count ${ack_range_count} cannot fit in the remaining ${buf.len - offset} bytes')
	}

	mut ranges := []AckRange{cap: int(ack_range_count) + 1}
	mut largest_in_range := largest_acknowledged
	mut smallest_in_range := largest_acknowledged - first_ack_range
	ranges << AckRange{
		smallest: smallest_in_range
		largest:  largest_in_range
	}

	for _ in 0 .. ack_range_count {
		gap, ng := decode_varint(buf[offset..])!
		offset += ng
		range_length, nl := decode_varint(buf[offset..])!
		offset += nl

		// smallest_in_range must be large enough to subtract (gap+2) from
		// without underflowing -- a malicious/corrupt gap that would send
		// this below zero is a malformed frame, not a value to silently
		// wrap on.
		if smallest_in_range < gap + 2 {
			return error('quic: ACK frame: gap ${gap} underflows the previous range (smallest so far: ${smallest_in_range})')
		}
		largest_in_range = smallest_in_range - gap - 2
		if range_length > largest_in_range {
			return error('quic: ACK frame: ack range length ${range_length} exceeds the range\'s own largest packet number ${largest_in_range}')
		}
		smallest_in_range = largest_in_range - range_length
		ranges << AckRange{
			smallest: smallest_in_range
			largest:  largest_in_range
		}
	}

	mut ecn_counts := ?EcnCounts(none)
	if has_ecn_counts {
		ect0, ne0 := decode_varint(buf[offset..])!
		offset += ne0
		ect1, ne1 := decode_varint(buf[offset..])!
		offset += ne1
		ecn_ce, ne2 := decode_varint(buf[offset..])!
		offset += ne2
		ecn_counts = EcnCounts{
			ect0:   ect0
			ect1:   ect1
			ecn_ce: ecn_ce
		}
	}

	return QuicFrame(AckFrame{
		largest_acknowledged: largest_acknowledged
		ack_delay:            ack_delay
		ranges:               ranges
		ecn_counts:           ecn_counts
	}), offset
}

fn parse_crypto_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	crypto_offset, n1 := decode_varint(buf[offset..])!
	offset += n1
	length, n2 := decode_varint(buf[offset..])!
	offset += n2
	if u64(offset) + length > u64(buf.len) {
		return error('quic: CRYPTO frame: length ${length} exceeds remaining buffer')
	}
	data := buf[offset..offset + int(length)].clone()
	offset += int(length)
	return QuicFrame(CryptoFrame{
		offset: crypto_offset
		data:   data
	}), offset
}

fn parse_connection_close_frame(buf []u8, start int, is_application_error bool) !(QuicFrame, int) {
	mut offset := start
	error_code, n1 := decode_varint(buf[offset..])!
	offset += n1

	mut frame_type := u64(0)
	if !is_application_error {
		ft, n2 := decode_varint(buf[offset..])!
		offset += n2
		frame_type = ft
	}

	reason_len, n3 := decode_varint(buf[offset..])!
	offset += n3
	if u64(offset) + reason_len > u64(buf.len) {
		return error('quic: CONNECTION_CLOSE frame: reason phrase length ${reason_len} exceeds remaining buffer')
	}
	reason := buf[offset..offset + int(reason_len)].bytestr()
	offset += int(reason_len)

	return QuicFrame(ConnectionCloseFrame{
		is_application_error: is_application_error
		error_code:           error_code
		frame_type:           frame_type
		reason:               reason
	}), offset
}

// parse_frames parses every frame filling `buf` (a packet's already
// AEAD-decrypted payload), in order, until the buffer is fully consumed.
pub fn parse_frames(buf []u8) ![]QuicFrame {
	mut frames := []QuicFrame{}
	mut offset := 0
	for offset < buf.len {
		frame, n := parse_frame(buf[offset..])!
		frames << frame
		offset += n
	}
	return frames
}

// encode_ack_frame serializes an ACK frame from its already-resolved
// ranges (largest-first, non-overlapping, matching AckFrame.ranges'
// shape), deriving the wire's Largest Acknowledged / First ACK Range /
// Gap / ACK Range Length encoding.
pub fn encode_ack_frame(ranges []AckRange, ack_delay u64, ecn_counts ?EcnCounts) ![]u8 {
	if ranges.len == 0 {
		return error('quic: encode_ack_frame: at least one range is required')
	}
	for i in 1 .. ranges.len {
		if ranges[i - 1].smallest < ranges[i].largest + 2 {
			return error('quic: encode_ack_frame: ranges[${i - 1}] and ranges[${i}] are not properly separated (need at least one unacknowledged packet number between them)')
		}
	}

	typ := if _ := ecn_counts { frame_type_ack_ecn } else { frame_type_ack }
	mut out := encode_varint(typ)!
	out << encode_varint(ranges[0].largest)!
	out << encode_varint(ack_delay)!
	out << encode_varint(u64(ranges.len - 1))!
	out << encode_varint(ranges[0].largest - ranges[0].smallest)!

	for i in 1 .. ranges.len {
		gap := ranges[i - 1].smallest - ranges[i].largest - 2
		range_length := ranges[i].largest - ranges[i].smallest
		out << encode_varint(gap)!
		out << encode_varint(range_length)!
	}

	if ecn := ecn_counts {
		out << encode_varint(ecn.ect0)!
		out << encode_varint(ecn.ect1)!
		out << encode_varint(ecn.ecn_ce)!
	}

	return out
}

// encode_crypto_frame serializes a CRYPTO frame.
pub fn encode_crypto_frame(offset u64, data []u8) ![]u8 {
	mut out := encode_varint(frame_type_crypto)!
	out << encode_varint(offset)!
	out << encode_varint(u64(data.len))!
	out << data
	return out
}

// encode_connection_close_frame serializes a CONNECTION_CLOSE frame.
// `frame_type` is ignored (encoded as 0) when `is_application_error` is
// true, matching the application-level variant's wire shape (RFC 9000
// §19.19, second form).
pub fn encode_connection_close_frame(is_application_error bool, error_code u64, frame_type u64, reason string) ![]u8 {
	typ := if is_application_error {
		frame_type_connection_close_application
	} else {
		frame_type_connection_close_transport
	}
	mut out := encode_varint(typ)!
	out << encode_varint(error_code)!
	if !is_application_error {
		out << encode_varint(frame_type)!
	}
	reason_bytes := reason.bytes()
	out << encode_varint(u64(reason_bytes.len))!
	out << reason_bytes
	return out
}
