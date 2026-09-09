module quic

// QpackErrorCode is the RFC 9204 §6/§8.3 "HTTP/3 Error Codes" additions for
// QPACK, used as the QUIC application-protocol error code when QPACK
// decoding or an encoder/decoder-stream instruction fails. Exact wire
// values transcribed directly from §6 / Table 3 (§8.3), not recalled --
// mirrors `h3_error.v`'s `H3ErrorCode` shape exactly (sibling checked
// before writing this).
pub enum QpackErrorCode {
	decompression_failed = 0x0200
	encoder_stream_error = 0x0201
	decoder_stream_error = 0x0202
}

// code returns the wire value of e as a u64, ready to carry as a QUIC
// application-protocol error code (RFC 9000 §20.2) in a CONNECTION_CLOSE
// frame.
pub fn (e QpackErrorCode) code() u64 {
	return u64(e)
}
