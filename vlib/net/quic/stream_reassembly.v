module quic

// Per-stream offset-ordered reassembly (RFC 9000 §2.2, §4.5). Mirrors
// crypto_stream.v's already-proven validated-append + promote_ready
// design (same overlap-tolerance and mismatch-detection reasoning applies
// identically to STREAM data as to CRYPTO data), extended with the one
// genuine difference: a stream has an explicit FINAL SIZE, learned from
// either a FIN-carrying STREAM frame or a RESET_STREAM frame, which must
// be reconciled against everything already received or buffered
// (FINAL_SIZE_ERROR on mismatch, RFC 9000 §4.5).

// max_stream_buffered_bytes bounds how much data (consumed + out-of-order
// pending) a single StreamReassembler will ever accept, for the same DoS
// reason crypto_stream.v's own limit exists -- a peer sending arbitrarily
// far-future STREAM offsets could otherwise exhaust memory. 1 MiB is
// generous for a single stream while still bounded; real flow control
// (flow_control.v) additionally caps this via the advertised
// max_stream_data limit long before this ceiling would ever matter in
// practice -- this is a hard backstop, not the primary defense.
pub const max_stream_buffered_bytes = u64(1) << 20

// max_stream_pending_fragments mirrors
// max_crypto_stream_pending_fragments's own reasoning: bounds how many
// DISTINCT out-of-order fragments can accumulate, independent of the
// byte-range cap above.
pub const max_stream_pending_fragments = 64

struct StreamDataFragment {
	offset u64
	data   []u8
}

@[heap]
pub struct StreamReassembler {
mut:
	received   []u8
	pending    []StreamDataFragment
	final_size ?u64
}

pub fn new_stream_reassembler() &StreamReassembler {
	return &StreamReassembler{}
}

pub fn (r &StreamReassembler) consumed_len() u64 {
	return u64(r.received.len)
}

pub fn (r &StreamReassembler) data() []u8 {
	return r.received
}

// is_finished reports whether every byte up to the (already learned)
// final size has been received.
pub fn (r &StreamReassembler) is_finished() bool {
	final := r.final_size or { return false }
	return u64(r.received.len) >= final
}

// append_or_validate: identical pattern to
// CryptoStreamReassembler.append_or_validate -- see that function's own
// doc comment (crypto_stream.v) for the full rationale; duplicated here
// (rather than shared) because the two reassemblers serve genuinely
// different structs with different extra concerns (final size tracking
// has no CRYPTO-stream equivalent), matching how the plan itself treats
// them as separate files.
fn (mut r StreamReassembler) append_or_validate(offset u64, data []u8) ! {
	consumed := u64(r.received.len)
	if offset > consumed {
		return error('quic: internal error: append_or_validate called with a gap (offset ${offset} > consumed ${consumed})')
	}
	overlap_len := consumed - offset
	if overlap_len > 0 {
		n := if u64(data.len) < overlap_len { u64(data.len) } else { overlap_len }
		if n > 0 && r.received[offset..offset + n] != data[..n] {
			return error('quic: STREAM data retransmission mismatch at offset ${offset}: overlapping bytes differ from previously received data')
		}
	}
	if u64(data.len) > overlap_len {
		r.received << data[overlap_len..]
	}
}

// note_final_size records the stream's final size (from a FIN-carrying
// STREAM frame's offset+length, or from RESET_STREAM's Final Size field)
// and validates it against everything already received or buffered. RFC
// 9000 §4.5: a final size SMALLER than data already received, or that
// DISAGREES with an already-established final size, is a
// FINAL_SIZE_ERROR. Idempotent: calling this again with the SAME value
// (e.g. a retransmitted FIN) is a no-op, not an error.
pub fn (mut r StreamReassembler) note_final_size(final_size u64) ! {
	if existing := r.final_size {
		if existing != final_size {
			return error('quic: FINAL_SIZE_ERROR: stream final size changed from ${existing} to ${final_size}')
		}
		return
	}
	if final_size < u64(r.received.len) {
		return error('quic: FINAL_SIZE_ERROR: final size ${final_size} is smaller than ${r.received.len} bytes already received')
	}
	for frag in r.pending {
		frag_end := frag.offset + u64(frag.data.len)
		if frag_end > final_size {
			return error('quic: FINAL_SIZE_ERROR: a previously received fragment extends to offset ${frag_end}, beyond final size ${final_size}')
		}
	}
	r.final_size = final_size
}

// add ingests one STREAM frame's (offset, data) into the reassembler.
// Frames may arrive out of order; both immediately-contiguous and
// out-of-order fragments are accepted and, for the latter, held until the
// gap before them closes.
pub fn (mut r StreamReassembler) add(offset u64, data []u8) ! {
	if data.len == 0 {
		return
	}
	end := offset + u64(data.len)
	if end > max_stream_buffered_bytes {
		return error('quic: STREAM data at offset ${offset} (length ${data.len}) would exceed the ${max_stream_buffered_bytes}-byte buffering limit for this stream')
	}
	if final := r.final_size {
		if end > final {
			return error('quic: FINAL_SIZE_ERROR: data at offset ${offset} (length ${data.len}) extends to ${end}, beyond final size ${final}')
		}
	}

	if offset <= u64(r.received.len) {
		r.append_or_validate(offset, data)!
	} else {
		if r.pending.len >= max_stream_pending_fragments {
			return error('quic: STREAM has too many out-of-order fragments buffered (limit ${max_stream_pending_fragments})')
		}
		r.pending << StreamDataFragment{
			offset: offset
			data:   data.clone()
		}
	}
	r.promote_ready()!
}

fn (mut r StreamReassembler) promote_ready() ! {
	mut advanced := true
	for advanced {
		advanced = false
		consumed := u64(r.received.len)
		for i, frag in r.pending {
			if frag.offset > consumed {
				continue
			}
			r.append_or_validate(frag.offset, frag.data)!
			r.pending.delete(i)
			advanced = true
			break
		}
	}
}
