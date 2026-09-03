module quic

// qpack_max_dynamic_table_capacity_bytes is this implementation's own upper
// bound on a QPACK dynamic table capacity. RFC 9204 names no specific
// number here, but §7.4's rationale ("implementation needs to ensure large
// values... do not create security weaknesses") applies to capacity itself
// exactly as much as it does to individual field values -- 64 MiB is far
// beyond any realistic HTTP header table need.
pub const qpack_max_dynamic_table_capacity_bytes = 64 * 1024 * 1024

// qpack_is_sensitive mirrors net.http's `h2_is_sensitive` (same security
// posture applied consistently across both compression schemes in this
// codebase, not a fresh unreviewed policy): RFC 9204 §7.1.3 names Cookie
// and Authorization as fields an encoder might choose never to index.
pub fn qpack_is_sensitive(name string) bool {
	return name == 'cookie' || name == 'authorization' || name == 'proxy-authorization'
}

// QpackUnackedSection is one encoded field section this encoder has sent
// that the decoder has not yet acknowledged (RFC 9204 §2.1.1, §2.2.2.1):
// which stream it went out on, and every dynamic-table entry it
// referenced, so those entries' references can be released together on
// Section Acknowledgment or Stream Cancellation.
struct QpackUnackedSection {
	stream_id  u64
	referenced []int
}

// QpackEncodedFieldSection is the result of encoding one field section:
// bytes for the encoder stream (any new dynamic-table entries this section
// caused to be inserted) and bytes for the request stream (the field
// section itself, prefix included).
pub struct QpackEncodedFieldSection {
pub:
	encoder_instructions []u8
	field_section        []u8
}

// QpackEncoder is a QPACK encoder's state (RFC 9204 §2.1): a dynamic
// table plus everything needed to track what the decoder has and has not
// yet acknowledged. Its `encode_field_section` uses the always-safe policy
// RFC 9204 §2.1.2 offers as one valid strategy -- "an encoder can avoid the
// risk of blocking by only referencing dynamic table entries that have
// been acknowledged" -- rather than the riskier "reference in-flight
// entries" strategy Appendix C's sample algorithm leaves as a choice. This
// means SETTINGS_QPACK_BLOCKED_STREAMS is trivially satisfied (this
// encoder never risks blocking a stream at all), a deliberate scope
// decision documented in this phase's conformance-matrix section, not an
// oversight.
@[heap]
pub struct QpackEncoder {
mut:
	dynamic_table           QpackDynamicTable
	known_received_count    int
	peer_max_table_capacity u64
	unacked                 []QpackUnackedSection
}

// new_qpack_encoder returns an encoder with an empty, zero-capacity
// dynamic table, as at the start of a connection (RFC 9204 §3.2.2: initial
// capacity is zero).
pub fn new_qpack_encoder() QpackEncoder {
	return QpackEncoder{}
}

// set_capacity sets the dynamic table's capacity (RFC 9204 §4.3.1),
// recording `peer_max_table_capacity` (from the peer's
// SETTINGS_QPACK_MAX_TABLE_CAPACITY, RFC 9204 §3.2.3) for future inserts
// to respect too. Returns the encoder-stream bytes to send. Errors if
// `new_capacity` exceeds either the peer's configured maximum or this
// implementation's own limit, or if reducing to it would require evicting
// an entry with an outstanding reference or unacknowledged status (RFC
// 9204 §2.1.1) -- self-found via Luna review: this call used to evict
// unconditionally, silently corrupting the encoder's own view of the table
// relative to what it had already told the peer.
pub fn (mut e QpackEncoder) set_capacity(new_capacity u64, peer_max_table_capacity u64) ![]u8 {
	if new_capacity > peer_max_table_capacity {
		return error('qpack: capacity ${new_capacity} exceeds peer max ${peer_max_table_capacity}')
	}
	if new_capacity > u64(qpack_max_dynamic_table_capacity_bytes) {
		return error('qpack: capacity ${new_capacity} exceeds implementation limit')
	}
	if !e.dynamic_table.can_set_capacity(int(new_capacity), e.known_received_count) {
		return error('qpack: reducing capacity to ${new_capacity} would evict a referenced or unacknowledged entry')
	}
	e.peer_max_table_capacity = peer_max_table_capacity
	e.dynamic_table.set_capacity(int(new_capacity))
	return encode_qpack_set_dynamic_table_capacity(new_capacity)
}

// note_section_acknowledged processes a Section Acknowledgment received on
// the decoder stream (RFC 9204 §4.4.1, §2.1.4): releases every dynamic-
// table reference the earliest unacknowledged field section on
// `stream_id` held, and advances Known Received Count if that section's
// own Required Insert Count was higher than what this encoder already
// believed acknowledged. Errors (QPACK_DECODER_STREAM_ERROR, per §4.4.1)
// if `stream_id` has no outstanding unacknowledged section.
pub fn (mut e QpackEncoder) note_section_acknowledged(stream_id u64) ! {
	mut idx := -1
	for i, s in e.unacked {
		if s.stream_id == stream_id {
			idx = i
			break
		}
	}
	if idx == -1 {
		return error_with_code('qpack: Section Acknowledgment for stream ${stream_id} with nothing outstanding',
			int(QpackErrorCode.decoder_stream_error))
	}
	section := e.unacked[idx]
	mut max_ref := -1
	for abs in section.referenced {
		e.dynamic_table.release_ref(abs) or {}
		if abs > max_ref {
			max_ref = abs
		}
	}
	if max_ref + 1 > e.known_received_count {
		e.known_received_count = max_ref + 1
	}
	e.unacked.delete(idx)
}

// note_stream_cancelled processes a Stream Cancellation received on the
// decoder stream (RFC 9204 §4.4.2, §2.2.2.2): releases every dynamic-table
// reference every unacknowledged field section on `stream_id` held (there
// can be more than one, e.g. trailers), WITHOUT advancing Known Received
// Count (the RFC is explicit: "An encoder cannot infer from this
// instruction that any updates to the dynamic table have been received").
pub fn (mut e QpackEncoder) note_stream_cancelled(stream_id u64) {
	mut kept := []QpackUnackedSection{}
	for s in e.unacked {
		if s.stream_id != stream_id {
			kept << s
			continue
		}
		for abs in s.referenced {
			e.dynamic_table.release_ref(abs) or {}
		}
	}
	e.unacked = kept
}

// note_insert_count_increment processes an Insert Count Increment received
// on the decoder stream (RFC 9204 §4.4.3): advances Known Received Count
// by `increment`. Errors (QPACK_DECODER_STREAM_ERROR) on an increment of
// zero, or one that would push Known Received Count past the number of
// entries this encoder has actually inserted -- checked in u64 space
// before narrowing, since `increment` arrives straight off the wire.
pub fn (mut e QpackEncoder) note_insert_count_increment(increment u64) ! {
	if increment == 0 {
		return error_with_code('qpack: Insert Count Increment of zero',
			int(QpackErrorCode.decoder_stream_error))
	}
	new_count := u64(e.known_received_count) + increment
	if new_count > u64(e.dynamic_table.insert_count()) {
		return error_with_code('qpack: Insert Count Increment exceeds entries actually inserted',
			int(QpackErrorCode.decoder_stream_error))
	}
	e.known_received_count = int(new_count)
}

// encode_field_section encodes `lines` for `stream_id` (RFC 9204 §2.1,
// Appendix C, adapted to the always-acknowledged-only policy described on
// `QpackEncoder`'s doc comment). For each line NOT marked `never_index`: an
// exact static match is used, else an exact match among already-
// acknowledged dynamic entries. Otherwise (including every `never_index`
// line) a literal is emitted, using a NAME reference (static or
// acknowledged-dynamic -- available regardless of `never_index`, since
// only a fully indexed representation is withheld) when available, and a
// new dynamic-table entry is inserted for future reuse whenever
// `can_insert` allows it (never for a `never_index` line). Errors only if
// the encoded prefix's Required Insert Count cannot be represented (RFC
// 9204 §4.5.1.1) -- unreachable via this policy's own `can_insert` gate in
// practice, but `encode_ric` is `pub` and this propagates its contract
// rather than assuming its one caller can never hit it.
pub fn (mut e QpackEncoder) encode_field_section(stream_id u64, lines []QpackFieldLine) !QpackEncodedFieldSection {
	mut instructions := []u8{}
	mut stream_buf := []u8{}
	mut referenced := []int{}
	mut required_insert_count := u64(0)
	base := e.dynamic_table.insert_count()

	for line in lines {
		// Effective never_index is the caller's own flag OR'd with this
		// encoder's own sensitivity check -- mirrors `h2_hpack.v`'s
		// `encode_field`, which applies `h2_is_sensitive` unconditionally
		// regardless of any caller input (HPACK's `H2HeaderField` doesn't
		// even expose a never-index field to the caller). Keeping BOTH here
		// is strictly safer than either alone: a caller can still opt a
		// field in explicitly, and a caller that forgets to flag a sensitive
		// field is protected anyway, the same defense-in-depth the sibling
		// gives every HPACK caller automatically.
		never_index := line.never_index || qpack_is_sensitive(line.name)

		// A fully indexed representation (exact static OR dynamic match) is
		// withheld for a never_index line, matching `encode_field`'s own
		// `if exact && !sensitive` gate (even a static exact match is
		// withheld), while a NAME-only static reference, checked separately
		// below, stays available regardless of never_index/sensitivity.
		if !never_index {
			if static_idx := qpack_static_find(line.name, line.value) {
				stream_buf << encode_indexed_static(u64(static_idx))
				continue
			}
			if abs := e.dynamic_table.find_exact(line.name, line.value, e.known_received_count) {
				e.dynamic_table.add_ref(abs) or {}
				referenced << abs
				if u64(abs) + 1 > required_insert_count {
					required_insert_count = u64(abs) + 1
				}
				rel := u64(base) - 1 - u64(abs)
				stream_buf << encode_indexed_dynamic_relative(rel)
				continue
			}
		}

		mut name_is_static := false
		mut name_index := u64(0)
		mut have_name_ref := false
		if static_name_idx := qpack_static_find_name(line.name) {
			name_is_static = true
			name_index = u64(static_name_idx)
			have_name_ref = true
		} else if !never_index {
			if dyn_name_idx := e.dynamic_table.find_name(line.name, e.known_received_count) {
				name_is_static = false
				name_index = u64(dyn_name_idx)
				have_name_ref = true
			}
		}

		mut inserted_abs := -1
		if !never_index && e.dynamic_table.can_insert(line.name, line.value, e.known_received_count) {
			// Capture insert_count BEFORE mutating: the Insert-With-Name-Reference
			// instruction's relative index is resolved by a decoder BEFORE this
			// instruction's own resulting insert takes effect (RFC 9204 §3.2.5,
			// Figure 2's "most recently inserted" is a snapshot taken at
			// processing time, not after), so using post-insert insert_count()
			// here would be off by one.
			pre_insert_count := e.dynamic_table.insert_count()
			if abs := e.dynamic_table.insert(line.name, line.value) {
				inserted_abs = abs
				if name_is_static {
					instructions << encode_qpack_insert_with_name_ref(true, name_index, line.value)
				} else if have_name_ref {
					rel := u64(pre_insert_count - 1) - name_index
					instructions << encode_qpack_insert_with_name_ref(false, rel, line.value)
				} else {
					instructions << encode_qpack_insert_with_literal_name(line.name, line.value)
				}
			}
		}

		if inserted_abs != -1 {
			e.dynamic_table.add_ref(inserted_abs) or {}
			referenced << inserted_abs
			if u64(inserted_abs) + 1 > required_insert_count {
				required_insert_count = u64(inserted_abs) + 1
			}
			// This is a FIELD LINE representation referencing an entry inserted
			// during this very call, so its absolute index is >= base -- that
			// needs post-Base indexing (§4.5.3), not relative-to-Base indexing
			// (§4.5.2, only valid for absolute index < Base).
			post_index := u64(inserted_abs) - u64(base)
			stream_buf << encode_indexed_dynamic_post_base(post_index)
			continue
		}

		if have_name_ref {
			if name_is_static {
				stream_buf << encode_literal_with_name_ref(true, never_index, name_index,
					line.value)
			} else {
				// This is STILL a reference to the dynamic table (for the NAME),
				// exactly like the exact-match and post-insert cases above -- it
				// must be tracked the same way: add_ref to protect the entry from
				// eviction before this section is acknowledged, record it in
				// `referenced` so note_section_acknowledged/note_stream_cancelled
				// can release it, and fold it into Required Insert Count so the
				// encoded prefix actually reflects that this section depends on
				// it (RFC 9204 §2.1.2: RIC is "one larger than the largest
				// absolute index of all referenced dynamic table entries" --
				// unconditionally, not only for entries this call itself
				// inserted). Skipping this produced a RIC that didn't cover a
				// reference the field section actually made, which a decoder
				// correctly rejects as QPACK_DECOMPRESSION_FAILED (RFC 9204
				// §2.2.3) -- self-found via Luna review, Phase-R reproduced
				// before this fix.
				e.dynamic_table.add_ref(int(name_index)) or {}
				referenced << int(name_index)
				if name_index + 1 > required_insert_count {
					required_insert_count = name_index + 1
				}
				// Field-line context: convert the absolute index `find_name` returned
				// to a Base-relative index (§4.5.4) -- distinct from the
				// insert-count-relative context the encoder instruction above uses.
				rel := u64(base) - 1 - name_index
				stream_buf << encode_literal_with_name_ref(false, never_index, rel, line.value)
			}
		} else {
			stream_buf << encode_literal_with_literal_name(never_index, line.name, line.value)
		}
	}

	if referenced.len > 0 {
		e.unacked << QpackUnackedSection{
			stream_id:  stream_id
			referenced: referenced
		}
	}

	prefix := encode_field_section_prefix(required_insert_count, u64(base),
		e.peer_max_table_capacity)!
	mut field_section := []u8{}
	field_section << prefix
	field_section << stream_buf
	return QpackEncodedFieldSection{
		encoder_instructions: instructions
		field_section:        field_section
	}
}
