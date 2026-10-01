module quic

// CRYPTO frame reassembly (RFC 9000 §19.6). Each encryption level (Initial,
// Handshake, 1-RTT -- 0-RTT is out of scope, see PROGRESS.md) has its own
// INDEPENDENT CRYPTO stream offset space; callers must use one
// CryptoStreamReassembler per level, never share one across levels. CRYPTO
// frames may arrive out of order and a retransmission may legitimately
// overlap previously-received bytes (a sender MUST NOT ever retransmit
// DIFFERENT bytes at the same offset -- that would corrupt the TLS
// handshake); this reassembler validates that invariant directly at the
// point an overlap is observed, rather than letting a violation surface
// only as a confusing downstream transcript-hash or Finished-MAC mismatch.

// max_crypto_stream_buffered_bytes bounds how much data (consumed +
// out-of-order pending) a single CryptoStreamReassembler will ever accept
// at one encryption level, keyed off the highest byte offset any admitted
// frame may claim. CRYPTO frames are NOT subject to QUIC's ordinary stream
// flow control (RFC 9000 §7.5), so nothing else in this module caps this --
// without a bound here, a malicious or buggy peer could send arbitrarily
// far-future CRYPTO offsets and exhaust memory before the handshake ever
// completes or fails. 64 KiB is generously larger than any realistic TLS
// 1.3 handshake flight (ClientHello/ServerHello/EncryptedExtensions/
// Certificate chain/CertificateVerify/Finished all comfortably fit within
// low tens of KB even for a sizeable certificate chain).
pub const max_crypto_stream_buffered_bytes = u64(65536)

// max_crypto_stream_pending_fragments bounds how many DISTINCT
// out-of-order fragments may accumulate in one CryptoStreamReassembler's
// pending list at once -- a separate concern from
// max_crypto_stream_buffered_bytes (which bounds the offset RANGE a
// fragment may claim, not how many small non-overlapping fragments can
// exist within that range). 64 is generous headroom for genuine network
// reordering across a single handshake's CRYPTO stream.
pub const max_crypto_stream_pending_fragments = 64

struct CryptoFragment {
	offset u64
	data   []u8
}

@[heap]
pub struct CryptoStreamReassembler {
mut:
	received []u8
	pending  []CryptoFragment
}

// new_crypto_stream_reassembler allocates an empty reassembler for one
// encryption level's CRYPTO stream (see this file's module-level doc
// comment for the one-per-level requirement).
pub fn new_crypto_stream_reassembler() &CryptoStreamReassembler {
	return &CryptoStreamReassembler{}
}

// consumed_len returns how many bytes, starting from stream offset 0, are
// currently contiguous and available via data().
pub fn (r &CryptoStreamReassembler) consumed_len() u64 {
	return u64(r.received.len)
}

// data returns the currently-contiguous prefix of the reassembled stream,
// as an independent COPY -- mutating the returned slice must never be able
// to corrupt r.received, since V's plain array assignment shares backing
// storage rather than copying it. Unlike a typical stream reader, CRYPTO
// frame offsets are never relative to a "read cursor" -- they are absolute
// from the start of the encryption level's handshake -- so this is a
// growing snapshot, not a destructive drain.
pub fn (r &CryptoStreamReassembler) data() []u8 {
	return r.received.clone()
}

// append_or_validate appends the not-yet-consumed suffix of (offset, data)
// to r.received, having first verified that any overlapping prefix agrees
// byte-for-byte with what is already there (RFC 9000 §19.6). Requires
// offset <= consumed_len() -- promoting a fragment that still has a gap
// before it is a caller bug, not a reachable-from-the-wire condition (add's
// offset<=consumed check, and promote_ready's own scan, both enforce this
// before ever calling here).
fn (mut r CryptoStreamReassembler) append_or_validate(offset u64, data []u8) ! {
	consumed := u64(r.received.len)
	if offset > consumed {
		return error('quic: internal error: append_or_validate called with a gap (offset ${offset} > consumed ${consumed})')
	}
	overlap_len := consumed - offset
	if overlap_len > 0 {
		n := if u64(data.len) < overlap_len { u64(data.len) } else { overlap_len }
		if n > 0 && r.received[offset..offset + n] != data[..n] {
			return error('quic: CRYPTO stream retransmission mismatch at offset ${offset}: overlapping bytes differ from previously received data')
		}
	}
	if u64(data.len) > overlap_len {
		r.received << data[overlap_len..]
	}
}

// add ingests one CRYPTO frame's (offset, data) into the reassembler.
// Frames may arrive out of order; both immediately-contiguous and
// out-of-order fragments are accepted and, for the latter, held until the
// gap before them closes.
pub fn (mut r CryptoStreamReassembler) add(offset u64, data []u8) ! {
	if data.len == 0 {
		return
	}
	// Checked BEFORE computing `end`: offset+data.len is u64 arithmetic, and
	// an offset near max_u64 wraps the sum back down to a small value,
	// silently passing an `end > max_crypto_stream_buffered_bytes` check
	// computed from the wrapped result (Codex P3, pullrequestreview-4840201604).
	// Rejecting on `offset` alone first makes the later addition provably
	// overflow-free, since max_crypto_stream_buffered_bytes is far below
	// u64's range.
	if offset > max_crypto_stream_buffered_bytes {
		return error('quic: CRYPTO stream data at offset ${offset} (length ${data.len}) would exceed the ${max_crypto_stream_buffered_bytes}-byte buffering limit for one encryption level')
	}
	end := offset + u64(data.len)
	if end > max_crypto_stream_buffered_bytes {
		return error('quic: CRYPTO stream data at offset ${offset} (length ${data.len}) would exceed the ${max_crypto_stream_buffered_bytes}-byte buffering limit for one encryption level')
	}

	if offset <= u64(r.received.len) {
		r.append_or_validate(offset, data)!
	} else {
		r.merge_or_add_pending(offset, data)!
	}
	r.promote_ready()!
}

// merge_or_add_pending merges (offset, data) into every existing r.pending
// entry it overlaps OR touches (shares a byte, or leaves no gap between
// them), validating that any shared bytes agree byte-for-byte -- exactly
// like append_or_validate does for the received-vs-new-data case -- then
// replaces the consumed entries with one fragment spanning their union.
//
// This is what keeps ORDINARY retransmission of an out-of-order CRYPTO
// range from exhausting max_crypto_stream_pending_fragments: RFC 9000 never
// requires a sender to retransmit CRYPTO data with byte-identical frame
// boundaries, so a peer resending the same underlying bytes with a
// one-byte-earlier offset each time (a completely ordinary shape under loss
// recovery, not an attack) must be recognized as the SAME logical range,
// not a fresh distinct fragment every time. Full-containment duplicates
// (this function's predecessor, pending_fully_covers, only handled that one
// shape) are just the special case where the union equals an existing
// entry unchanged.
//
// A genuinely disjoint (non-overlapping, non-touching) fragment still adds
// a new pending entry and is still subject to
// max_crypto_stream_pending_fragments -- checked BEFORE any mutation, so a
// rejected fragment never leaves r.pending in a partially-mutated state.
fn (mut r CryptoStreamReassembler) merge_or_add_pending(offset u64, data []u8) ! {
	mut merged_offset := offset
	mut merged_end := offset + u64(data.len)
	mut merged_data := data.clone()
	mut consumed := []int{}

	for i, frag in r.pending {
		frag_end := frag.offset + u64(frag.data.len)
		touches := frag.offset <= merged_end && frag_end >= merged_offset
		if !touches {
			continue
		}
		overlap_start := if frag.offset > merged_offset { frag.offset } else { merged_offset }
		overlap_end := if frag_end < merged_end { frag_end } else { merged_end }
		if overlap_end > overlap_start {
			// Plain (not unsafe) slicing: overlap_start/overlap_end are
			// already-validated indices within both merged_data's and
			// frag.data's bounds (derived from the max()/min() clamps
			// above), and this is an equality comparison, not a per-packet
			// hot path -- unlike packet_protection.v/header_protection.v's
			// use of unsafe{} for that reason, there is no performance case
			// for skipping the bounds-checked, cloned slice here.
			a := merged_data[overlap_start - merged_offset..overlap_end - merged_offset].clone()
			b := frag.data[overlap_start - frag.offset..overlap_end - frag.offset].clone()
			if a != b {
				return error('quic: CRYPTO stream retransmission mismatch at offset ${overlap_start}: disagrees with an already-pending out-of-order fragment')
			}
		}
		if frag.offset < merged_offset {
			mut new_data := frag.data[..merged_offset - frag.offset].clone()
			new_data << merged_data
			merged_data = new_data.clone()
			merged_offset = frag.offset
		}
		if frag_end > merged_end {
			merged_data << frag.data[merged_end - frag.offset..]
			merged_end = frag_end
		}
		consumed << i
	}

	// max_crypto_stream_buffered_bytes bounds the OFFSET range a fragment
	// may claim, but says nothing about how many DISTINCT out-of-order
	// fragments can accumulate within that range -- a peer sending many
	// tiny, genuinely disjoint fragments could otherwise inflate
	// r.pending's entry count far beyond anything a real lossy/reordering
	// network would produce. Only a genuinely disjoint fragment (consumed
	// is empty -- it merged with nothing) can grow r.pending's count, so
	// only that case is checked against the cap; a merge always keeps the
	// count the same or reduces it.
	if consumed.len == 0 && r.pending.len >= max_crypto_stream_pending_fragments {
		return error('quic: CRYPTO stream has too many out-of-order fragments buffered (limit ${max_crypto_stream_pending_fragments})')
	}

	// Delete consumed entries in reverse index order so an earlier delete
	// never shifts the index of one still to be removed.
	for i := consumed.len - 1; i >= 0; i-- {
		r.pending.delete(consumed[i])
	}
	r.pending << CryptoFragment{
		offset: merged_offset
		data:   merged_data
	}
}

// promote_ready repeatedly scans r.pending for any fragment whose offset
// has become reachable now that r.received has grown, promoting it via the
// same validated-append path append_or_validate uses for fresh arrivals.
// The restart-after-each-promotion loop exists because merge_or_add_pending
// already keeps every entry in r.pending pairwise non-overlapping/
// non-touching by construction -- so promoting one entry can grow
// r.received just far enough to newly reach a DIFFERENT, previously-
// out-of-reach pending entry, which the next iteration must then also
// promote, and so on until nothing further is reachable.
fn (mut r CryptoStreamReassembler) promote_ready() ! {
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
