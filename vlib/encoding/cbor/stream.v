module cbor

import io

// Stream I/O wrappers over the standard `io.Reader` / `io.Writer`
// interfaces. Use these for files, sockets, pipes — anywhere the
// payload doesn't fit cleanly in a single `[]u8`.

// encode_to serialises `val` into an internal buffer, then writes the
// bytes to `w` in a loop until everything is accepted. Errors on the
// first I/O failure.
pub fn encode_to[T](val T, mut w io.Writer, opts EncodeOpts) ! {
	bytes := encode[T](val, opts)!
	mut written := 0
	for written < bytes.len {
		n := w.write(bytes[written..])!
		if n == 0 {
			return error('cbor: writer stopped accepting bytes at ${written}/${bytes.len}')
		}
		written += n
	}
}

// decode_from reads bytes from `r` until EOF (or until
// `DecodeOpts.max_stream_bytes` is hit) and decodes a single top-level
// value. For multi-value streams, use `Unpacker` directly on a
// pre-buffered slice.
//
// Always set `max_stream_bytes` on untrusted readers — otherwise a peer
// that never sends EOF blocks the call forever.
pub fn decode_from[T](mut r io.Reader, opts DecodeOpts) !T {
	bounded := opts.max_stream_bytes > 0
	mut buf := []u8{cap: 4096}
	for {
		if bounded && buf.len >= opts.max_stream_bytes {
			return error('cbor: stream exceeded max_stream_bytes (${opts.max_stream_bytes})')
		}
		slot_len := if bounded {
			cap_left := opts.max_stream_bytes - buf.len
			if cap_left < 4096 {
				cap_left
			} else {
				4096
			}
		} else {
			4096
		}
		mut slot := []u8{len: slot_len}
		// Treat io.Eof as the legitimate end of the stream; any other
		// reader error must propagate so callers don't mistake a
		// transport failure for a successful (truncated) decode. The
		// unbounded branch used to delegate to `io.read_all`, which
		// also swallows non-EOF errors — same loop here keeps the
		// strict semantic regardless of whether `max_stream_bytes` is set.
		n := r.read(mut slot) or {
			if err is io.Eof {
				break
			}
			return err
		}
		if n == 0 {
			break
		}
		buf << slot[..n]
	}
	return decode[T](buf, opts)!
}

// pack_to is the streaming sibling of `encode_to`, for users who built
// their payload manually via the `Packer` API. Errors if any
// indefinite-length container is still open — emitting half-closed
// CBOR would produce a payload no decoder can parse.
pub fn (mut p Packer) pack_to(mut w io.Writer) ! {
	if !p.is_complete() {
		return error('cbor: pack_to called with an open indefinite-length item — close it with pack_break first')
	}
	bytes := p.bytes()
	mut written := 0
	for written < bytes.len {
		n := w.write(bytes[written..])!
		if n == 0 {
			return error('cbor: writer stopped at ${written}/${bytes.len}')
		}
		written += n
	}
}
