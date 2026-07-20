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

pub fn new_crypto_stream_reassembler() &CryptoStreamReassembler {
	return &CryptoStreamReassembler{}
}

// consumed_len returns how many bytes, starting from stream offset 0, are
// currently contiguous and available via data().
pub fn (r &CryptoStreamReassembler) consumed_len() u64 {
	return u64(r.received.len)
}

// data returns the currently-contiguous prefix of the reassembled stream.
// Unlike a typical stream reader, CRYPTO frame offsets are never relative
// to a "read cursor" -- they are absolute from the start of the encryption
// level's handshake -- so this is a growing snapshot, not a destructive
// drain.
pub fn (r &CryptoStreamReassembler) data() []u8 {
	return r.received
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
	end := offset + u64(data.len)
	if end > max_crypto_stream_buffered_bytes {
		return error('quic: CRYPTO stream data at offset ${offset} (length ${data.len}) would exceed the ${max_crypto_stream_buffered_bytes}-byte buffering limit for one encryption level')
	}

	if offset <= u64(r.received.len) {
		r.append_or_validate(offset, data)!
	} else {
		// max_crypto_stream_buffered_bytes bounds the OFFSET range a
		// fragment may claim, but says nothing about how many DISTINCT
		// out-of-order fragments can accumulate within that range -- a
		// peer sending many tiny, non-overlapping fragments could
		// otherwise inflate r.pending's entry count (and per-entry
		// overhead) far beyond anything a real lossy/reordering network
		// would produce for one handshake's CRYPTO stream. Reordering
		// across dozens of packets is already generous headroom; this
		// caps it well short of "attacker-chosen number of allocations".
		if r.pending.len >= max_crypto_stream_pending_fragments {
			return error('quic: CRYPTO stream has too many out-of-order fragments buffered (limit ${max_crypto_stream_pending_fragments})')
		}
		r.pending << CryptoFragment{
			offset: offset
			data:   data.clone()
		}
	}
	r.promote_ready()!
}

// promote_ready repeatedly scans r.pending for any fragment whose offset
// has become reachable now that r.received has grown, promoting it via the
// same validated-append path append_or_validate uses for fresh arrivals.
// This is what closes the gap where two out-of-order fragments overlap
// EACH OTHER before either has touched r.received: whichever is promoted
// second is still checked -- via append_or_validate -- against whatever the
// first one already wrote, since both go through the identical function.
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
