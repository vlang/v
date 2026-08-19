module quic

// QpackDynamicTableEntry is one entry currently held in a QPACK dynamic
// table (RFC 9204 §3.2). `ref_count` is the number of outstanding
// unacknowledged field-section references to this entry (§2.1.1) -- it is
// meaningful only on the encoder's own table; a decoder's mirrored table
// never consults it (see this file's module doc comment).
pub struct QpackDynamicTableEntry {
pub mut:
	name      string
	value     string
	ref_count int
}

// qpack_entry_size is the size RFC 9204 §3.2.1 assigns a dynamic table
// entry for accounting purposes: name length + value length + 32 bytes of
// fixed overhead, measured on the UN-Huffman-encoded name/value.
pub fn qpack_entry_size(name string, value string) int {
	return name.len + value.len + 32
}

// QpackDynamicTable is a QPACK dynamic table (RFC 9204 §3.2): a FIFO of
// entries addressed by a permanent absolute index (§3.2.4), with capacity-
// bounded eviction from the oldest end (§3.2.2).
//
// The SAME struct backs both an encoder's and a decoder's table, but their
// `insert` obligations differ (RFC 9204 draws this distinction implicitly,
// by describing eviction from the encoder's point of view in §2.1.1/§3.2.2
// while requiring the decoder to independently apply the identical
// instructions it receives): a decoder mirrors whatever the encoder
// instructs unconditionally (`insert`/`set_capacity` here), while an
// encoder MUST first confirm via `can_insert` that it would not have to
// evict a non-evictable entry to make room -- if `can_insert` returns
// false, the encoder must not call `insert` at all (fall back to a literal
// representation instead). `can_insert` and the ref-count methods
// (`add_ref`/`release_ref`) are therefore only ever meaningful when this
// table is playing the encoder role; a decoder's table never calls them.
@[heap]
pub struct QpackDynamicTable {
mut:
	entries  []QpackDynamicTableEntry // FIFO; entries[0] is the oldest entry still present
	dropped  int                      // count of entries evicted so far == absolute index of entries[0], if any
	capacity int
	cur_size int
}

// insert_count is the total number of entries ever inserted (RFC 9204
// "Insert Count"), including ones since evicted -- the absolute index the
// NEXT inserted entry will receive.
pub fn (t &QpackDynamicTable) insert_count() int {
	return t.dropped + t.entries.len
}

// size is the dynamic table's current total size in bytes (RFC 9204
// §3.2.1), the sum of `qpack_entry_size` over every entry still present.
pub fn (t &QpackDynamicTable) size() int {
	return t.cur_size
}

// capacity is the dynamic table's current capacity in bytes (RFC 9204
// §3.2.2), the upper bound `size()` is kept at or under via eviction.
pub fn (t &QpackDynamicTable) capacity() int {
	return t.capacity
}

// can_insert reports whether inserting `name`/`value` right now would
// require evicting a non-evictable entry (RFC 9204 §2.1.1: an entry with
// absolute index >= `known_received_count`, or with any outstanding
// reference, cannot be evicted). An encoder MUST check this before
// `insert`-ing; a decoder never needs to, since it always applies
// instructions its peer encoder already validated this way.
pub fn (t &QpackDynamicTable) can_insert(name string, value string, known_received_count int) bool {
	needed := qpack_entry_size(name, value)
	if needed > t.capacity {
		return false
	}
	mut freed := t.capacity - t.cur_size
	for i := 0; i < t.entries.len && freed < needed; i++ {
		e := t.entries[i]
		if t.dropped + i >= known_received_count || e.ref_count > 0 {
			return false
		}
		freed += qpack_entry_size(e.name, e.value)
	}
	return freed >= needed
}

// insert adds a new entry, first evicting entries from the oldest end
// (unconditionally -- see this struct's module doc comment for who may
// safely call this without first checking `can_insert`) until it fits
// within capacity. Returns the new entry's absolute index. Errors if the
// entry alone is larger than the table's capacity (RFC 9204 §3.2.2: the
// decoder MUST treat this as QPACK_ENCODER_STREAM_ERROR -- the caller maps
// this generic error to that code, since the QPACK error taxonomy isn't
// this file's concern).
pub fn (mut t QpackDynamicTable) insert(name string, value string) !int {
	needed := qpack_entry_size(name, value)
	if needed > t.capacity {
		return error('qpack: entry size ${needed} exceeds dynamic table capacity ${t.capacity}')
	}
	for t.cur_size + needed > t.capacity && t.entries.len > 0 {
		evicted := t.entries[0]
		t.cur_size -= qpack_entry_size(evicted.name, evicted.value)
		t.entries.delete(0)
		t.dropped++
	}
	t.entries << QpackDynamicTableEntry{
		name:  name
		value: value
	}
	t.cur_size += needed
	return t.dropped + t.entries.len - 1
}

// duplicate reinserts the entry at encoder-instruction-context relative
// index `rel_index` (RFC 9204 §4.3.4) as a new entry with a fresh absolute
// index, without needing its name/value re-transmitted on the wire by the
// caller.
pub fn (mut t QpackDynamicTable) duplicate(rel_index u64) !int {
	abs := t.resolve_relative_from_insert_count(rel_index)!
	e := t.get(abs)!
	return t.insert(e.name, e.value)
}

// can_set_capacity reports whether reducing capacity to `new_capacity`
// right now would require evicting a non-evictable entry (RFC 9204 §2.1.1:
// an entry with absolute index >= `known_received_count`, or with any
// outstanding reference, cannot be evicted) -- the same evictability rule
// `can_insert` simulates before an insertion, applied here to a capacity
// reduction instead. An encoder MUST check this before calling
// `set_capacity` with a smaller value; mirroring `insert`/`can_insert`,
// `set_capacity` itself does not check, so the caller controls whether a
// failed check means "reject" or some other recovery.
pub fn (t &QpackDynamicTable) can_set_capacity(new_capacity int, known_received_count int) bool {
	over := t.cur_size - new_capacity
	mut freed := 0
	for i := 0; i < t.entries.len && freed < over; i++ {
		e := t.entries[i]
		if t.dropped + i >= known_received_count || e.ref_count > 0 {
			return false
		}
		freed += qpack_entry_size(e.name, e.value)
	}
	return freed >= over
}

// set_capacity applies a new dynamic table capacity (RFC 9204 §4.3.1/
// §3.2.2), evicting entries from the oldest end until size fits, which can
// clear the table entirely at capacity 0. Whether `new_capacity` itself is
// within the peer's configured maximum (§3.2.3) is the caller's check --
// this table has no notion of that external limit. Callers reducing
// capacity MUST check `can_set_capacity` first (see its doc comment) --
// this evicts unconditionally, exactly like `insert`.
pub fn (mut t QpackDynamicTable) set_capacity(new_capacity int) {
	t.capacity = new_capacity
	for t.cur_size > t.capacity && t.entries.len > 0 {
		evicted := t.entries[0]
		t.cur_size -= qpack_entry_size(evicted.name, evicted.value)
		t.entries.delete(0)
		t.dropped++
	}
}

// get returns the entry at absolute index `abs_index` (RFC 9204 §3.2.4).
// Errors if it has already been evicted or has not been inserted yet --
// the caller maps this to QPACK_DECOMPRESSION_FAILED or
// QPACK_ENCODER_STREAM_ERROR per RFC 9204 §2.2.3, depending on which
// stream the reference came from.
pub fn (t &QpackDynamicTable) get(abs_index int) !QpackDynamicTableEntry {
	if abs_index < t.dropped || abs_index >= t.insert_count() {
		return error('qpack: absolute index ${abs_index} references an evicted or nonexistent entry')
	}
	return t.entries[abs_index - t.dropped]
}

// add_ref records one more outstanding unacknowledged reference to the
// entry at `abs_index` (RFC 9204 §2.1.1), blocking its eviction until a
// matching `release_ref`. Only meaningful on an encoder's own table.
pub fn (mut t QpackDynamicTable) add_ref(abs_index int) ! {
	if abs_index < t.dropped || abs_index >= t.insert_count() {
		return error('qpack: cannot reference evicted or nonexistent entry ${abs_index}')
	}
	t.entries[abs_index - t.dropped].ref_count++
}

// release_ref removes one outstanding reference to the entry at
// `abs_index`, recorded by an earlier `add_ref` (RFC 9204 §2.1.1,
// §2.2.2.2: released by Section Acknowledgment or Stream Cancellation).
// A no-op if the entry has no outstanding references (never negative).
pub fn (mut t QpackDynamicTable) release_ref(abs_index int) ! {
	if abs_index < t.dropped || abs_index >= t.insert_count() {
		return error('qpack: cannot release reference on evicted or nonexistent entry ${abs_index}')
	}
	if t.entries[abs_index - t.dropped].ref_count > 0 {
		t.entries[abs_index - t.dropped].ref_count--
	}
}

// find_exact returns the absolute index of the most recently inserted
// entry whose name AND value both match, among entries with absolute index
// less than `before` (exclusive). Used by an encoder that wants to avoid
// referencing an entry the decoder may not have acknowledged yet.
pub fn (t &QpackDynamicTable) find_exact(name string, value string, before int) ?int {
	for i := t.entries.len - 1; i >= 0; i-- {
		abs := t.dropped + i
		if abs >= before {
			continue
		}
		e := t.entries[i]
		if e.name == name && e.value == value {
			return abs
		}
	}
	return none
}

// find_name is `find_exact`'s name-only counterpart, for a literal field
// line that can still reference the dynamic table for its name.
pub fn (t &QpackDynamicTable) find_name(name string, before int) ?int {
	for i := t.entries.len - 1; i >= 0; i-- {
		abs := t.dropped + i
		if abs >= before {
			continue
		}
		if t.entries[i].name == name {
			return abs
		}
	}
	return none
}

// resolve_relative_from_insert_count resolves a relative index as used in
// ENCODER INSTRUCTIONS (RFC 9204 §3.2.5, Figure 2): 0 refers to the most
// recently inserted entry, shifting as more entries are inserted. Distinct
// from `resolve_relative_from_base` below -- conflating the two reference
// points is the most likely transcription error for this section, so they
// are deliberately separate functions rather than one parameterized by a
// "which context" flag.
//
// `rel_index` arrives straight from the wire (via `decode_prefixed_int`,
// itself capped at `qpack_max_prefixed_int` < 2^62) and so must not be
// narrowed to `int` before it is bounds-checked: the comparisons below
// happen entirely in u64 space against `t.insert_count()`/`t.dropped`
// (always small, since they only ever grow by one real insertion at a
// time) before the final, by-then-safe narrow to `int` for indexing.
pub fn (t &QpackDynamicTable) resolve_relative_from_insert_count(rel_index u64) !int {
	ic := u64(t.insert_count())
	if rel_index >= ic {
		return error('qpack: relative index ${rel_index} references an evicted or nonexistent entry')
	}
	abs64 := ic - 1 - rel_index
	if abs64 < u64(t.dropped) {
		return error('qpack: relative index ${rel_index} references an evicted or nonexistent entry')
	}
	return int(abs64)
}

// resolve_relative_from_base resolves a relative index as used in FIELD
// LINE REPRESENTATIONS (RFC 9204 §3.2.5, Figure 3): 0 refers to the entry
// with absolute index (Base - 1), stable regardless of when the encoded
// field section is processed relative to encoder-stream instructions. See
// `resolve_relative_from_insert_count`'s doc comment for why this is a
// separate function, and for why the u64 arithmetic below is bounds-checked
// before narrowing (both `rel_index` and `base` are wire-derived).
pub fn (t &QpackDynamicTable) resolve_relative_from_base(rel_index u64, base u64) !int {
	if rel_index >= base {
		return error('qpack: relative index ${rel_index} (base ${base}) references an evicted or nonexistent entry')
	}
	abs64 := base - 1 - rel_index
	ic := u64(t.insert_count())
	if abs64 < u64(t.dropped) || abs64 >= ic {
		return error('qpack: relative index ${rel_index} (base ${base}) references an evicted or nonexistent entry')
	}
	return int(abs64)
}

// resolve_post_base resolves a post-Base index (RFC 9204 §3.2.6, Figure 4):
// 0 refers to the entry with absolute index equal to Base, increasing in
// the same direction as absolute index. Used for entries inserted while
// processing the same (or another) field section, referenced after Base
// was fixed. `base + post_index` cannot overflow u64: both operands are
// individually capped below `qpack_max_prefixed_int` (< 2^62) by
// `decode_prefixed_int`, so their sum is safely under 2^63.
pub fn (t &QpackDynamicTable) resolve_post_base(post_index u64, base u64) !int {
	abs64 := base + post_index
	ic := u64(t.insert_count())
	if abs64 < u64(t.dropped) || abs64 >= ic {
		return error('qpack: post-base index ${post_index} (base ${base}) references an evicted or nonexistent entry')
	}
	return int(abs64)
}
