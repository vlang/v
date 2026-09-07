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
	// A wire-legal raw_ack_delay (up to max_varint, i.e. up to 62
	// significant bits) combined with a wire-legal ack_delay_exponent (up
	// to 20 per RFC 9000 §18.2) can shift up to 82 bits of significance --
	// far past u64's 64 bits. A naive `<<` silently wraps rather than
	// erroring, which would make an enormous peer-claimed delay look
	// artificially small (even zero) to callers, defeating any downstream
	// max_ack_delay cap. Saturate instead of wrapping: an oversized delay
	// should read as "as large as representable", never as "small".
	if ack_delay_exponent >= 64 {
		return if raw_ack_delay == 0 { u64(0) } else { max_u64 }
	}
	if raw_ack_delay > (max_u64 >> ack_delay_exponent) {
		return max_u64
	}
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

// StreamFrame represents a STREAM frame (type 0x08-0x0f, RFC 9000 §19.8):
// a chunk of one stream's byte data at `offset`, optionally marking the
// end of the stream (`fin`). Reassembling multiple (possibly out-of-order,
// possibly overlapping) StreamFrames into a contiguous per-stream byte
// stream is stream_reassembly.v's job, not this one's.
pub struct StreamFrame {
pub:
	stream_id u64
	offset    u64
	fin       bool
	data      []u8
}

// ResetStreamFrame represents a RESET_STREAM frame (type 0x04, RFC 9000
// §19.4): the sender is abandoning the send side of `stream_id`, and
// `final_size` is the exact total size that stream would have reached had
// it not been reset -- reconciled against any data already received via
// StreamReassembler.note_final_size (FINAL_SIZE_ERROR on mismatch).
pub struct ResetStreamFrame {
pub:
	stream_id  u64
	error_code u64
	final_size u64
}

// StopSendingFrame represents a STOP_SENDING frame (type 0x05, RFC 9000
// §19.5): a request that the peer abandon sending on `stream_id`.
pub struct StopSendingFrame {
pub:
	stream_id  u64
	error_code u64
}

// MaxDataFrame represents a MAX_DATA frame (type 0x10, RFC 9000 §19.9):
// raises the CONNECTION-level limit on how much the receiver of this
// frame may send in total, across all streams.
pub struct MaxDataFrame {
pub:
	maximum_data u64
}

// MaxStreamDataFrame represents a MAX_STREAM_DATA frame (type 0x11, RFC
// 9000 §19.10): raises the STREAM-level limit on `stream_id`.
pub struct MaxStreamDataFrame {
pub:
	stream_id           u64
	maximum_stream_data u64
}

// MaxStreamsFrame represents a MAX_STREAMS frame (type 0x12 bidirectional,
// 0x13 unidirectional, RFC 9000 §19.11): raises how many concurrent
// streams of `direction` the receiver of this frame may have open.
pub struct MaxStreamsFrame {
pub:
	direction       StreamDirection
	maximum_streams u64
}

// DataBlockedFrame represents a DATA_BLOCKED frame (type 0x14, RFC 9000
// §19.12): informs the peer the sender wanted to send more but was
// blocked by the connection-level flow control limit `maximum_data`.
pub struct DataBlockedFrame {
pub:
	maximum_data u64
}

// NewTokenFrame represents a NEW_TOKEN frame (type 0x07, RFC 9000 §19.7):
// sent only by a server, providing a token the client MAY present on a
// FUTURE connection's Initial packet for address validation (and,
// separately, 0-RTT). This module implements neither 0-RTT nor token-
// carrying reconnection (net.quic's own documented v1 scope), so the
// token is parsed and kept only long enough to size the frame correctly
// on the wire -- never stored or reused. Parsing it at all (rather than
// treating the type as unrecognized) is still required: real servers
// (confirmed: Google's QUIC endpoints) send this immediately after the
// handshake as standard practice, unrelated to whether the CLIENT ever
// intends to use it, and RFC 9000 §12.4's frame type table has no
// allowance for an implementation to reject a frame it merely doesn't
// act on.
pub struct NewTokenFrame {
pub:
	token []u8
}

// NewConnectionIdFrame represents a NEW_CONNECTION_ID frame (type 0x18,
// RFC 9000 §19.15): the peer offering an additional connection ID this
// endpoint MAY switch to. Parsed and otherwise ignored -- net.quic
// implements neither an active-CID pool nor connection migration (v1
// scope, PROGRESS.md), and never switches away from the one DCID
// established at the handshake. As with NewTokenFrame, parsing (not
// merely tolerating on the wire) is required regardless: real servers
// send this as standard practice independent of whether the client ever
// migrates, per RFC 9000 §5.1.1's own recommendation to keep a pool of
// several ready.
pub struct NewConnectionIdFrame {
pub:
	sequence_number       u64
	retire_prior_to       u64
	connection_id         []u8
	stateless_reset_token []u8
}

// RetireConnectionIdFrame represents a RETIRE_CONNECTION_ID frame (type
// 0x19, RFC 9000 §19.16): the peer retiring one of ITS OWN previously
// issued connection IDs (the ones the peer uses to address US, from this
// endpoint's own NEW_CONNECTION_ID frames -- which net.quic never sends,
// having no active-CID pool of its own to issue from). Parsed and
// otherwise ignored, same rationale as NewConnectionIdFrame.
pub struct RetireConnectionIdFrame {
pub:
	sequence_number u64
}

// PathChallengeFrame/PathResponseFrame represent PATH_CHALLENGE (0x1a)/
// PATH_RESPONSE (0x1b) frames (RFC 9000 §19.17/§19.18): 8 bytes of
// sender-chosen data. RFC 9000 §8.2.1's MUST-respond-with-PATH_RESPONSE
// requirement is NOT implemented here (net.quic never migrates and has
// never been observed to receive an unprompted PATH_CHALLENGE from a
// real server in practice) -- parsed so the connection survives one
// arriving, not acted upon. Tracked as a known gap in PROGRESS.md rather
// than silently presented as full RFC 9000 §8.2 path-validation support.
pub struct PathChallengeFrame {
pub:
	data []u8
}

pub struct PathResponseFrame {
pub:
	data []u8
}

// StreamDataBlockedFrame represents a STREAM_DATA_BLOCKED frame (type
// 0x15, RFC 9000 §19.13): same as DataBlockedFrame, but for one stream's
// limit.
pub struct StreamDataBlockedFrame {
pub:
	stream_id           u64
	maximum_stream_data u64
}

// StreamsBlockedFrame represents a STREAMS_BLOCKED frame (type 0x16
// bidirectional, 0x17 unidirectional, RFC 9000 §19.14): informs the peer
// the sender wanted to open another stream of `direction` but was blocked
// by the max_streams limit.
pub struct StreamsBlockedFrame {
pub:
	direction       StreamDirection
	maximum_streams u64
}

// HandshakeDoneFrame represents a HANDSHAKE_DONE frame (type 0x1e, RFC 9000
// §19.20): sent only by a server, only once, to signal handshake
// confirmation (RFC 9001 §4.1.2) -- carries no fields. A client MUST treat
// receipt of one as a connection error of type PROTOCOL_VIOLATION (RFC 9000
// §19.20); parse_frame itself has no connection-role awareness to enforce
// that, so it is the caller's (Phase 9 QuicConn's) job, same division as
// every other role-dependent check this module defers (see coalesce.v's
// analogous note).
pub struct HandshakeDoneFrame {}

pub type QuicFrame = AckFrame
	| ConnectionCloseFrame
	| CryptoFrame
	| DataBlockedFrame
	| HandshakeDoneFrame
	| MaxDataFrame
	| MaxStreamDataFrame
	| MaxStreamsFrame
	| NewConnectionIdFrame
	| NewTokenFrame
	| PaddingFrame
	| PathChallengeFrame
	| PathResponseFrame
	| PingFrame
	| ResetStreamFrame
	| RetireConnectionIdFrame
	| StopSendingFrame
	| StreamDataBlockedFrame
	| StreamFrame
	| StreamsBlockedFrame

const frame_type_padding = u64(0x00)
const frame_type_ping = u64(0x01)
const frame_type_ack = u64(0x02)
const frame_type_ack_ecn = u64(0x03)
const frame_type_reset_stream = u64(0x04)
const frame_type_stop_sending = u64(0x05)
const frame_type_crypto = u64(0x06)
const frame_type_new_token = u64(0x07)
const frame_type_stream_base = u64(0x08) // 0x08-0x0f, OFF/LEN/FIN bits in the low 3 bits

const frame_type_max_data = u64(0x10)
const frame_type_max_stream_data = u64(0x11)
const frame_type_max_streams_bidi = u64(0x12)
const frame_type_max_streams_uni = u64(0x13)
const frame_type_data_blocked = u64(0x14)
const frame_type_stream_data_blocked = u64(0x15)
const frame_type_streams_blocked_bidi = u64(0x16)
const frame_type_streams_blocked_uni = u64(0x17)
const frame_type_new_connection_id = u64(0x18)
const frame_type_retire_connection_id = u64(0x19)
const frame_type_path_challenge = u64(0x1a)
const frame_type_path_response = u64(0x1b)
const frame_type_connection_close_transport = u64(0x1c)
const frame_type_connection_close_application = u64(0x1d)
const frame_type_handshake_done = u64(0x1e)

// max_connection_id_length is RFC 9000 §17.2's own connection ID length
// bound (a single byte's worth by construction, but the RFC additionally
// caps it here) -- NEW_CONNECTION_ID's Length field (§19.15) MUST NOT
// exceed it.
const max_connection_id_length = 20

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

	if typ == frame_type_reset_stream {
		return parse_reset_stream_frame(buf, typ_len)
	}

	if typ == frame_type_stop_sending {
		return parse_stop_sending_frame(buf, typ_len)
	}

	if typ == frame_type_ack || typ == frame_type_ack_ecn {
		return parse_ack_frame(buf, typ_len, typ == frame_type_ack_ecn)
	}

	if typ == frame_type_crypto {
		return parse_crypto_frame(buf, typ_len)
	}

	if typ == frame_type_new_token {
		return parse_new_token_frame(buf, typ_len)
	}

	if typ >= frame_type_stream_base && typ <= frame_type_stream_base + 7 {
		return parse_stream_frame(buf, typ_len, u8(typ))
	}

	if typ == frame_type_max_data {
		return parse_max_data_frame(buf, typ_len)
	}

	if typ == frame_type_max_stream_data {
		return parse_max_stream_data_frame(buf, typ_len)
	}

	if typ == frame_type_max_streams_bidi || typ == frame_type_max_streams_uni {
		return parse_max_streams_frame(buf, typ_len, typ == frame_type_max_streams_uni)
	}

	if typ == frame_type_data_blocked {
		return parse_data_blocked_frame(buf, typ_len)
	}

	if typ == frame_type_stream_data_blocked {
		return parse_stream_data_blocked_frame(buf, typ_len)
	}

	if typ == frame_type_streams_blocked_bidi || typ == frame_type_streams_blocked_uni {
		return parse_streams_blocked_frame(buf, typ_len, typ == frame_type_streams_blocked_uni)
	}

	if typ == frame_type_new_connection_id {
		return parse_new_connection_id_frame(buf, typ_len)
	}

	if typ == frame_type_retire_connection_id {
		return parse_retire_connection_id_frame(buf, typ_len)
	}

	if typ == frame_type_path_challenge {
		return parse_path_challenge_or_response_frame(buf, typ_len, false)
	}

	if typ == frame_type_path_response {
		return parse_path_challenge_or_response_frame(buf, typ_len, true)
	}

	if typ == frame_type_connection_close_transport
		|| typ == frame_type_connection_close_application {
		return parse_connection_close_frame(buf, typ_len, typ == frame_type_connection_close_application)
	}

	if typ == frame_type_handshake_done {
		return QuicFrame(HandshakeDoneFrame{}), typ_len
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
		largest: largest_in_range
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
			return error("quic: ACK frame: ack range length ${range_length} exceeds the range's own largest packet number ${largest_in_range}")
		}
		smallest_in_range = largest_in_range - range_length
		ranges << AckRange{
			smallest: smallest_in_range
			largest: largest_in_range
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
			ect0: ect0
			ect1: ect1
			ecn_ce: ecn_ce
		}
	}

	return QuicFrame(AckFrame{
		largest_acknowledged: largest_acknowledged
		ack_delay: ack_delay
		ranges: ranges
		ecn_counts: ecn_counts
	}), offset
}

// parse_new_token_frame parses a NEW_TOKEN frame (RFC 9000 §19.7):
// Type (i) = 0x07, Token Length (i), Token (..). A zero-length token is
// syntactically legal per the grammar (no MUST-be-nonzero requirement in
// the RFC text) -- rejecting it here would be inventing a restriction the
// spec doesn't impose, so it is accepted, matching parse_crypto_frame's
// identical tolerance for a zero-length CRYPTO frame.
fn parse_new_token_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	length, n1 := decode_varint(buf[offset..])!
	offset += n1
	if u64(offset) + length > u64(buf.len) {
		return error('quic: NEW_TOKEN frame: length ${length} exceeds remaining buffer')
	}
	token := buf[offset..offset + int(length)].clone()
	offset += int(length)
	return QuicFrame(NewTokenFrame{
		token: token
	}), offset
}

// stateless_reset_token_length is RFC 9000 §19.15's fixed 128-bit (16
// byte) Stateless Reset Token field width, carried by every
// NEW_CONNECTION_ID frame.
const stateless_reset_token_length = 16

// path_challenge_data_length is RFC 9000 §19.17/§19.18's fixed 8-byte
// data field width, shared by PATH_CHALLENGE and PATH_RESPONSE.
const path_challenge_data_length = 8

// parse_new_connection_id_frame parses a NEW_CONNECTION_ID frame (RFC
// 9000 §19.15): Sequence Number (i), Retire Prior To (i), Length (8),
// Connection ID (8..160), Stateless Reset Token (128). "Retire Prior To"
// MUST NOT exceed "Sequence Number" (RFC 9000 §19.15) -- checked here even
// though the value itself is otherwise unused (NewConnectionIdFrame's own
// doc comment), since accepting an out-of-order value would silently let
// a malformed frame through as if it were well-formed.
fn parse_new_connection_id_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	sequence_number, n1 := decode_varint(buf[offset..])!
	offset += n1
	retire_prior_to, n2 := decode_varint(buf[offset..])!
	offset += n2
	if retire_prior_to > sequence_number {
		return error('quic: NEW_CONNECTION_ID frame: retire_prior_to ${retire_prior_to} exceeds sequence_number ${sequence_number} (RFC 9000 §19.15)')
	}
	if offset >= buf.len {
		return error('quic: NEW_CONNECTION_ID frame: missing Length field')
	}
	cid_len := int(buf[offset])
	offset += 1
	// RFC 9000 §19.15: "Values less than 1 and greater than 20 are invalid
	// and MUST be treated as a connection error of type
	// FRAME_ENCODING_ERROR." A zero-length connection ID is a distinct,
	// separately-named invalid case from the upper bound below, not just
	// an edge of it -- both ends of the range need their own check.
	if cid_len < 1 {
		return error('quic: NEW_CONNECTION_ID frame: connection ID length must be at least 1 (RFC 9000 §19.15)')
	}
	if cid_len > max_connection_id_length {
		return error('quic: NEW_CONNECTION_ID frame: connection ID length ${cid_len} exceeds the ${max_connection_id_length}-byte limit (RFC 9000 §19.15)')
	}
	if offset + cid_len + stateless_reset_token_length > buf.len {
		return error('quic: NEW_CONNECTION_ID frame: connection ID/stateless reset token exceed remaining buffer')
	}
	connection_id := buf[offset..offset + cid_len].clone()
	offset += cid_len
	stateless_reset_token := buf[offset..offset + stateless_reset_token_length].clone()
	offset += stateless_reset_token_length
	return QuicFrame(NewConnectionIdFrame{
		sequence_number: sequence_number
		retire_prior_to: retire_prior_to
		connection_id: connection_id
		stateless_reset_token: stateless_reset_token
	}), offset
}

// parse_retire_connection_id_frame parses a RETIRE_CONNECTION_ID frame
// (RFC 9000 §19.16): Sequence Number (i).
fn parse_retire_connection_id_frame(buf []u8, start int) !(QuicFrame, int) {
	sequence_number, n1 := decode_varint(buf[start..])!
	return QuicFrame(RetireConnectionIdFrame{
		sequence_number: sequence_number
	}), start + n1
}

// parse_path_challenge_or_response_frame parses a PATH_CHALLENGE (RFC
// 9000 §19.17) or PATH_RESPONSE (§19.18) frame -- identical wire shape,
// one fixed 8-byte Data field, differing only in which frame type they
// decode to.
fn parse_path_challenge_or_response_frame(buf []u8, start int, is_response bool) !(QuicFrame, int) {
	if start + path_challenge_data_length > buf.len {
		return error('quic: PATH_CHALLENGE/PATH_RESPONSE frame: data field exceeds remaining buffer')
	}
	data := buf[start..start + path_challenge_data_length].clone()
	end := start + path_challenge_data_length
	if is_response {
		return QuicFrame(PathResponseFrame{
			data: data
		}), end
	}
	return QuicFrame(PathChallengeFrame{
		data: data
	}), end
}

// encode_path_response_frame serializes a PATH_RESPONSE frame (RFC 9000
// §19.18): the same 8 bytes a peer's PATH_CHALLENGE carried, echoed back
// verbatim -- RFC 9000 §8.2.1: "an endpoint MUST respond by echoing the
// data contained in the PATH_CHALLENGE frame in a PATH_RESPONSE frame."
pub fn encode_path_response_frame(data []u8) ![]u8 {
	if data.len != path_challenge_data_length {
		return error('quic: encode_path_response_frame: data must be exactly ${path_challenge_data_length} bytes, got ${data.len}')
	}
	mut out := encode_varint(frame_type_path_response)!
	out << data
	return out
}

fn parse_crypto_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	crypto_offset, n1 := decode_varint(buf[offset..])!
	offset += n1
	length, n2 := decode_varint(buf[offset..])!
	offset += n2
	// RFC 9000 §19.6: "The largest offset delivered on a stream -- the sum
	// of the offset and data length -- cannot exceed 2^62-1." Each field is
	// individually a legal varint (up to max_varint on its own), so only an
	// explicit sum check catches an attacker-chosen offset near max_varint
	// paired with any nonzero length; u64 arithmetic here cannot itself
	// overflow (two values each under 2^62 sum to at most just under 2^63,
	// comfortably within u64's range), so this is a clean comparison, not
	// an overflow-safety concern in its own right.
	if crypto_offset + length > max_varint {
		return error('quic: CRYPTO frame: offset ${crypto_offset} + length ${length} exceeds the 2^62-1 varint limit (RFC 9000 §19.6)')
	}
	if u64(offset) + length > u64(buf.len) {
		return error('quic: CRYPTO frame: length ${length} exceeds remaining buffer')
	}
	data := buf[offset..offset + int(length)].clone()
	offset += int(length)
	return QuicFrame(CryptoFrame{
		offset: crypto_offset
		data: data
	}), offset
}

fn parse_reset_stream_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	stream_id, n1 := decode_varint(buf[offset..])!
	offset += n1
	error_code, n2 := decode_varint(buf[offset..])!
	offset += n2
	final_size, n3 := decode_varint(buf[offset..])!
	offset += n3
	return QuicFrame(ResetStreamFrame{
		stream_id: stream_id
		error_code: error_code
		final_size: final_size
	}), offset
}

fn parse_stop_sending_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	stream_id, n1 := decode_varint(buf[offset..])!
	offset += n1
	error_code, n2 := decode_varint(buf[offset..])!
	offset += n2
	return QuicFrame(StopSendingFrame{
		stream_id: stream_id
		error_code: error_code
	}), offset
}

// parse_stream_frame parses a STREAM frame given its already-decoded type
// byte (0x08-0x0f), whose low 3 bits carry the OFF/LEN/FIN flags. When the
// LEN bit is clear, the frame's data extends to the end of `buf` -- this
// correctly ends the enclosing packet's frame sequence when parse_frames
// walks off the end of the consumed buffer, since a STREAM frame without
// an explicit length is REQUIRED to be the last frame in its packet.
fn parse_stream_frame(buf []u8, start int, type_byte u8) !(QuicFrame, int) {
	off_bit := type_byte & 0x04 != 0
	len_bit := type_byte & 0x02 != 0
	fin := type_byte & 0x01 != 0

	mut offset := start
	stream_id, n1 := decode_varint(buf[offset..])!
	offset += n1

	mut stream_offset := u64(0)
	if off_bit {
		off, n2 := decode_varint(buf[offset..])!
		stream_offset = off
		offset += n2
	}

	mut data := []u8{}
	if len_bit {
		length, n3 := decode_varint(buf[offset..])!
		offset += n3
		// RFC 9000 §19.8 (identical requirement to §19.6's CRYPTO-frame
		// bound, see parse_crypto_frame): "The largest offset delivered on
		// a stream -- the sum of the offset and data length -- cannot
		// exceed 2^62-1." u64 arithmetic here cannot itself overflow (two
		// values each under 2^62 sum to at most just under 2^63).
		if stream_offset + length > max_varint {
			return error('quic: STREAM frame: offset ${stream_offset} + length ${length} exceeds the 2^62-1 varint limit (RFC 9000 §19.8)')
		}
		if u64(offset) + length > u64(buf.len) {
			return error('quic: STREAM frame: length ${length} exceeds remaining buffer')
		}
		data = buf[offset..offset + int(length)].clone()
		offset += int(length)
	} else {
		implicit_length := u64(buf.len - offset)
		if stream_offset + implicit_length > max_varint {
			return error('quic: STREAM frame: offset ${stream_offset} + length ${implicit_length} exceeds the 2^62-1 varint limit (RFC 9000 §19.8)')
		}
		data = buf[offset..].clone()
		offset = buf.len
	}

	return QuicFrame(StreamFrame{
		stream_id: stream_id
		offset: stream_offset
		fin: fin
		data: data
	}), offset
}

fn parse_max_data_frame(buf []u8, start int) !(QuicFrame, int) {
	maximum_data, n1 := decode_varint(buf[start..])!
	return QuicFrame(MaxDataFrame{
		maximum_data: maximum_data
	}), start + n1
}

fn parse_max_stream_data_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	stream_id, n1 := decode_varint(buf[offset..])!
	offset += n1
	maximum_stream_data, n2 := decode_varint(buf[offset..])!
	offset += n2
	return QuicFrame(MaxStreamDataFrame{
		stream_id: stream_id
		maximum_stream_data: maximum_stream_data
	}), offset
}

fn parse_max_streams_frame(buf []u8, start int, is_uni bool) !(QuicFrame, int) {
	maximum_streams, n1 := decode_varint(buf[start..])!
	// RFC 9000 §4.6: a MAX_STREAMS value above 2^60 would allow a stream ID
	// that cannot be expressed as a variable-length integer -- MUST be
	// treated as FRAME_ENCODING_ERROR. Mirrors the identical
	// max_initial_max_streams check already enforced on the transport
	// parameter (transport_parameters.v) for the same RFC requirement.
	if maximum_streams > max_initial_max_streams {
		return error('quic: MAX_STREAMS frame: value ${maximum_streams} exceeds the ${max_initial_max_streams} (2^60) limit (RFC 9000 §4.6)')
	}
	return QuicFrame(MaxStreamsFrame{
		direction: if is_uni {
			StreamDirection.unidirectional
		} else {
			StreamDirection.bidirectional
		}
		maximum_streams: maximum_streams
	}), start + n1
}

fn parse_data_blocked_frame(buf []u8, start int) !(QuicFrame, int) {
	maximum_data, n1 := decode_varint(buf[start..])!
	return QuicFrame(DataBlockedFrame{
		maximum_data: maximum_data
	}), start + n1
}

fn parse_stream_data_blocked_frame(buf []u8, start int) !(QuicFrame, int) {
	mut offset := start
	stream_id, n1 := decode_varint(buf[offset..])!
	offset += n1
	maximum_stream_data, n2 := decode_varint(buf[offset..])!
	offset += n2
	return QuicFrame(StreamDataBlockedFrame{
		stream_id: stream_id
		maximum_stream_data: maximum_stream_data
	}), offset
}

fn parse_streams_blocked_frame(buf []u8, start int, is_uni bool) !(QuicFrame, int) {
	maximum_streams, n1 := decode_varint(buf[start..])!
	// RFC 9000 §19.14: the same 2^60 cap as MAX_STREAMS (§4.6) -- carries the
	// identical "maximum stream count" semantic, so a value exceeding it
	// would likewise imply a stream ID inexpressible as a varint.
	if maximum_streams > max_initial_max_streams {
		return error('quic: STREAMS_BLOCKED frame: value ${maximum_streams} exceeds the ${max_initial_max_streams} (2^60) limit (RFC 9000 §19.14)')
	}
	return QuicFrame(StreamsBlockedFrame{
		direction: if is_uni {
			StreamDirection.unidirectional
		} else {
			StreamDirection.bidirectional
		}
		maximum_streams: maximum_streams
	}), start + n1
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
		error_code: error_code
		frame_type: frame_type
		reason: reason
	}), offset
}

// parse_frames parses every frame filling `buf` (a packet's already
// AEAD-decrypted payload), in order, until the buffer is fully consumed.
pub fn parse_frames(buf []u8) ![]QuicFrame {
	// RFC 9000 §12.4: "An endpoint MUST treat receipt of a packet
	// containing no frames as a connection error of type
	// PROTOCOL_VIOLATION." parse_frame (singular) already rejects an empty
	// buffer, but this loop's own `for offset < buf.len` guard never even
	// calls it when buf itself is empty, so that rejection must be
	// duplicated here rather than inherited from it.
	if buf.len == 0 {
		return error('quic: packet payload contains no frames (RFC 9000 §12.4: PROTOCOL_VIOLATION)')
	}
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
	for i, r in ranges {
		// RFC 9000 §12.3: packet numbers are bounded to 0..2^62-1. Checking
		// this BEFORE the separation-check arithmetic below matters: that
		// arithmetic (`largest + 2`, `smallest - largest - 2`) silently
		// wraps in u64 when an endpoint sits near max_u64, which can turn a
		// self-contradictory range ordering into what looks like a valid,
		// small encoded gap -- an out-of-range endpoint must be rejected
		// outright, not fed into arithmetic that can wrap around it.
		if r.largest > max_varint || r.smallest > max_varint {
			return error('quic: encode_ack_frame: ranges[${i}] has an endpoint exceeding the 2^62-1 packet-number limit (RFC 9000 §12.3)')
		}
		if r.largest < r.smallest {
			return error('quic: encode_ack_frame: ranges[${i}] has largest (${r.largest}) < smallest (${r.smallest})')
		}
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
	// RFC 9000 §19.6: the sum of offset and data length cannot exceed
	// 2^62-1, even though each is individually a legal varint on its own
	// (see parse_crypto_frame's mirrored check on the decode side).
	if offset + u64(data.len) > max_varint {
		return error('quic: encode_crypto_frame: offset ${offset} + length ${data.len} exceeds the 2^62-1 varint limit (RFC 9000 §19.6)')
	}
	mut out := encode_varint(frame_type_crypto)!
	out << encode_varint(offset)!
	out << encode_varint(u64(data.len))!
	out << data
	return out
}

// encode_reset_stream_frame serializes a RESET_STREAM frame.
pub fn encode_reset_stream_frame(stream_id u64, error_code u64, final_size u64) ![]u8 {
	mut out := encode_varint(frame_type_reset_stream)!
	out << encode_varint(stream_id)!
	out << encode_varint(error_code)!
	out << encode_varint(final_size)!
	return out
}

// encode_stop_sending_frame serializes a STOP_SENDING frame.
pub fn encode_stop_sending_frame(stream_id u64, error_code u64) ![]u8 {
	mut out := encode_varint(frame_type_stop_sending)!
	out << encode_varint(stream_id)!
	out << encode_varint(error_code)!
	return out
}

// encode_stream_frame serializes a STREAM frame. The OFF bit is included
// automatically whenever `offset != 0` (never needed for 0, and always
// correct to include when nonzero); `include_length`, however, is a
// genuine caller decision -- omitting the LEN field means this frame MUST
// be the last one in its packet (RFC 9000 §19.8), a packet-layout choice
// stream.v/flow_control.v's caller makes, not something inferable from the
// frame's own fields alone.
pub fn encode_stream_frame(stream_id u64, offset u64, data []u8, fin bool, include_length bool) ![]u8 {
	// RFC 9000 §19.8 (mirrors encode_crypto_frame's identical §19.6 check):
	// the sum of offset and data length must not exceed 2^62-1. Unlike the
	// PARSE-side check (both operands there come from decode_varint, which
	// inherently caps each at max_varint, so their sum can never overflow
	// u64), `offset` here is CALLER-supplied with no such inherent bound --
	// a caller passing e.g. offset near u64::MAX would make `offset +
	// u64(data.len)` itself wrap to a small value, silently passing an
	// `offset + length > max_varint` check. Checking `offset` alone first,
	// then rearranging the sum comparison into an overflow-safe
	// subtraction, avoids ever computing the (potentially wrapping) sum.
	if offset > max_varint {
		return error('quic: encode_stream_frame: offset ${offset} exceeds the 2^62-1 varint limit (RFC 9000 §19.8)')
	}
	if data.len > 0 && offset > max_varint - u64(data.len) {
		return error('quic: encode_stream_frame: offset ${offset} + length ${data.len} exceeds the 2^62-1 varint limit (RFC 9000 §19.8)')
	}
	mut type_bits := u8(frame_type_stream_base)
	if offset != 0 {
		type_bits |= 0x04
	}
	if include_length {
		type_bits |= 0x02
	}
	if fin {
		type_bits |= 0x01
	}
	mut out := encode_varint(u64(type_bits))!
	out << encode_varint(stream_id)!
	if offset != 0 {
		out << encode_varint(offset)!
	}
	if include_length {
		out << encode_varint(u64(data.len))!
	}
	out << data
	return out
}

// encode_max_data_frame serializes a MAX_DATA frame.
pub fn encode_max_data_frame(maximum_data u64) ![]u8 {
	mut out := encode_varint(frame_type_max_data)!
	out << encode_varint(maximum_data)!
	return out
}

// encode_max_stream_data_frame serializes a MAX_STREAM_DATA frame.
pub fn encode_max_stream_data_frame(stream_id u64, maximum_stream_data u64) ![]u8 {
	mut out := encode_varint(frame_type_max_stream_data)!
	out << encode_varint(stream_id)!
	out << encode_varint(maximum_stream_data)!
	return out
}

// encode_max_streams_frame serializes a MAX_STREAMS frame.
pub fn encode_max_streams_frame(direction StreamDirection, maximum_streams u64) ![]u8 {
	// RFC 9000 §4.6 (mirrors parse_max_streams_frame's identical check).
	if maximum_streams > max_initial_max_streams {
		return error('quic: encode_max_streams_frame: value ${maximum_streams} exceeds the ${max_initial_max_streams} (2^60) limit (RFC 9000 §4.6)')
	}
	typ := if direction == .unidirectional {
		frame_type_max_streams_uni
	} else {
		frame_type_max_streams_bidi
	}
	mut out := encode_varint(typ)!
	out << encode_varint(maximum_streams)!
	return out
}

// encode_data_blocked_frame serializes a DATA_BLOCKED frame.
pub fn encode_data_blocked_frame(maximum_data u64) ![]u8 {
	mut out := encode_varint(frame_type_data_blocked)!
	out << encode_varint(maximum_data)!
	return out
}

// encode_stream_data_blocked_frame serializes a STREAM_DATA_BLOCKED frame.
pub fn encode_stream_data_blocked_frame(stream_id u64, maximum_stream_data u64) ![]u8 {
	mut out := encode_varint(frame_type_stream_data_blocked)!
	out << encode_varint(stream_id)!
	out << encode_varint(maximum_stream_data)!
	return out
}

// encode_streams_blocked_frame serializes a STREAMS_BLOCKED frame.
pub fn encode_streams_blocked_frame(direction StreamDirection, maximum_streams u64) ![]u8 {
	// RFC 9000 §19.14 (mirrors parse_streams_blocked_frame's identical check).
	if maximum_streams > max_initial_max_streams {
		return error('quic: encode_streams_blocked_frame: value ${maximum_streams} exceeds the ${max_initial_max_streams} (2^60) limit (RFC 9000 §19.14)')
	}
	typ := if direction == .unidirectional {
		frame_type_streams_blocked_uni
	} else {
		frame_type_streams_blocked_bidi
	}
	mut out := encode_varint(typ)!
	out << encode_varint(maximum_streams)!
	return out
}

// encode_connection_close_frame serializes a CONNECTION_CLOSE frame.
// `frame_type` is ignored (the Frame Type field is OMITTED from the wire
// entirely, not encoded as a zero value) when `is_application_error` is
// true, matching the application-level variant's wire shape (RFC 9000
// §19.19, second form) -- a decoder parsing this back sees no such field
// on the wire either, which is why parse_frame's own
// ConnectionCloseFrame.frame_type defaults to 0 for that variant, rather
// than reading a zero varint that was never sent.
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
