module quic

// Per-stream offset-ordered reassembly (RFC 9000 §2.2, §4.5). Mirrors
// crypto_stream.v's already-proven validated-append + promote_ready
// design (same overlap-tolerance and mismatch-detection reasoning applies
// identically to STREAM data as to CRYPTO data), extended with the one
// genuine difference: a stream has an explicit FINAL SIZE, learned from
// either a FIN-carrying STREAM frame or a RESET_STREAM frame, which must
// be reconciled against everything already received or buffered
// (FINAL_SIZE_ERROR on mismatch, RFC 9000 §4.5).

// max_stream_buffered_bytes bounds how many bytes of BUFFERED-BUT-NOT-YET-
// CONSUMED data (the contiguous `received` window plus out-of-order
// `pending` fragments) a single StreamReassembler will ever hold at once --
// not the stream's total size. discard() lets a caller (the future
// application-read path, Phase 9) free consumed bytes, sliding this window
// forward as the stream progresses, so a stream of any total size can be
// handled as long as the UNCONSUMED portion at any one moment stays under
// this ceiling. 1 MiB is generous for that window while still bounded, for
// the same DoS reason crypto_stream.v's own limit exists -- a peer sending
// arbitrarily far-future STREAM offsets could otherwise exhaust memory.
// Real flow control (flow_control.v) additionally caps outstanding
// unconsumed bytes via the advertised max_stream_data limit long before
// this ceiling would ever matter in practice -- this is a hard backstop,
// not the primary defense.
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
	// base_offset is the stream-absolute offset of received[0] -- bytes
	// before this position have already been consumed and discarded, and
	// `received` holds only the window from base_offset onward.
	base_offset u64
	received    []u8
	pending     []StreamDataFragment
	final_size  ?u64
}

// new_stream_reassembler constructs an empty reassembler starting at
// stream offset 0.
pub fn new_stream_reassembler() &StreamReassembler {
	return &StreamReassembler{}
}

// consumed_len returns the STREAM-ABSOLUTE offset marking how much has been
// contiguously received so far. Unaffected by discard(): freeing already-
// consumed bytes doesn't change how much of the stream has actually
// arrived, only how much of it this reassembler still holds in memory.
pub fn (r &StreamReassembler) consumed_len() u64 {
	return r.base_offset + u64(r.received.len)
}

// data returns the currently-HELD window of contiguous bytes, starting at
// whatever offset base_offset last advanced to via discard() -- NOT
// necessarily the whole stream from its start.
pub fn (r &StreamReassembler) data() []u8 {
	return r.received
}

// discard drops bytes before `new_base` (a STREAM-ABSOLUTE offset) from the
// held window, freeing the memory for data the caller has already
// consumed -- this is what lets total stream size exceed
// max_stream_buffered_bytes, by keeping the UNCONSUMED window bounded
// instead of the whole stream. `new_base` must not exceed consumed_len()
// (discarding data not yet received would silently create a gap this
// reassembler can no longer detect). Idempotent/tolerant of a stale
// `new_base` at or before the current base_offset -- a no-op, not an error,
// since a caller re-confirming consumption it already reported is normal.
pub fn (mut r StreamReassembler) discard(new_base u64) ! {
	if new_base <= r.base_offset {
		return
	}
	if new_base > r.consumed_len() {
		return error('quic: internal error: discard(${new_base}) exceeds consumed_len ${r.consumed_len()}')
	}
	r.received = r.received[new_base - r.base_offset..].clone()
	r.base_offset = new_base
}

// is_finished reports whether every byte up to the (already learned)
// final size has been received.
pub fn (r &StreamReassembler) is_finished() bool {
	final := r.final_size or { return false }
	return r.consumed_len() >= final
}

// append_or_validate: identical pattern to
// CryptoStreamReassembler.append_or_validate -- see that function's own
// doc comment (crypto_stream.v) for the full rationale; duplicated here
// (rather than shared) because the two reassemblers serve genuinely
// different structs with different extra concerns (final size tracking
// has no CRYPTO-stream equivalent), matching how the plan itself treats
// them as separate files.
//
// `offset` may be BELOW base_offset -- a legitimate retransmission of data
// already consumed and discarded, which is tolerated silently (there is
// nothing left to validate it against, and a compliant peer retransmitting
// old bytes is normal, not a protocol violation) rather than erroring.
fn (mut r StreamReassembler) append_or_validate(offset u64, data []u8) ! {
	consumed := r.consumed_len()
	if offset > consumed {
		return error('quic: internal error: append_or_validate called with a gap (offset ${offset} > consumed ${consumed})')
	}
	mut off := offset
	mut skip := u64(0)
	if off < r.base_offset {
		skip = r.base_offset - off
		if skip >= u64(data.len) {
			return
		}
		off = r.base_offset
	}
	d := data[skip..]

	local_off := off - r.base_offset
	overlap_len := u64(r.received.len) - local_off
	if overlap_len > 0 {
		n := if u64(d.len) < overlap_len { u64(d.len) } else { overlap_len }
		if n > 0 && r.received[local_off..local_off + n] != d[..n] {
			return error('quic: STREAM data retransmission mismatch at offset ${off}: overlapping bytes differ from previously received data')
		}
	}
	if u64(d.len) > overlap_len {
		growth := u64(d.len) - overlap_len
		if u64(r.received.len) + growth > max_stream_buffered_bytes {
			return error('quic: STREAM data at offset ${offset} (length ${data.len}) would exceed the ${max_stream_buffered_bytes}-byte buffering limit for this stream (consume and discard() received data to free room)')
		}
		r.received << d[overlap_len..]
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
	consumed := r.consumed_len()
	if final_size < consumed {
		return error('quic: FINAL_SIZE_ERROR: final size ${final_size} is smaller than ${consumed} bytes already received')
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
// gap before them closes. The max_stream_buffered_bytes cap is enforced
// against the UNCONSUMED window (received + pending), not the frame's wire
// offset -- see append_or_validate for the received-side check and below
// for the pending-side one.
pub fn (mut r StreamReassembler) add(offset u64, data []u8) ! {
	if data.len == 0 {
		return
	}
	end := offset + u64(data.len)
	if final := r.final_size {
		if end > final {
			return error('quic: FINAL_SIZE_ERROR: data at offset ${offset} (length ${data.len}) extends to ${end}, beyond final size ${final}')
		}
	}

	if offset <= r.consumed_len() {
		r.append_or_validate(offset, data)!
	} else {
		if r.pending.len >= max_stream_pending_fragments {
			return error('quic: STREAM has too many out-of-order fragments buffered (limit ${max_stream_pending_fragments})')
		}
		mut pending_bytes := u64(0)
		for frag in r.pending {
			pending_bytes += u64(frag.data.len)
		}
		if pending_bytes + u64(data.len) > max_stream_buffered_bytes {
			return error('quic: STREAM out-of-order data at offset ${offset} (length ${data.len}) would exceed the ${max_stream_buffered_bytes}-byte buffering limit for this stream')
		}
		r.pending << StreamDataFragment{
			offset: offset
			data: data.clone()
		}
	}
	r.promote_ready()!
}

fn (mut r StreamReassembler) promote_ready() ! {
	mut advanced := true
	for advanced {
		advanced = false
		consumed := r.consumed_len()
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
