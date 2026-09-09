// Hardening tests: adversarial payloads, RFC 8949 strictness boundaries
// on both encode and decode, and round-trip invariants for the parts
// the rest of the suite doesn't cover (sub-second time, NaN dedup,
// canonical float, indef-string composition rules).
module main

import encoding.cbor
import encoding.hex
import io
import time

fn b(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

// ---------------------------------------------------------------------
// Decode bounds: a length argument can be up to u64::max on the wire,
// but the host's int is i32 on 32-bit targets. Naively casting the
// argument before bounds-checking aborts with `array.slice: invalid
// slice index (start>end)` or `negative .cap`. Every length read path
// must clamp before any int(...) cast.
// ---------------------------------------------------------------------

fn test_unpack_text_rejects_u64_max_length() {
	mut u := cbor.new_unpacker(b('7bffffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_text() {
		assert false, 'u64::max text length must not panic the decoder'
	}
}

fn test_unpack_bytes_rejects_u64_max_length() {
	mut u := cbor.new_unpacker(b('5bffffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_bytes() {
		assert false, 'u64::max bytes length must not panic the decoder'
	}
}

fn test_unpack_text_rejects_i32_overflow_length() {
	// 7a 80000000 = text(2^31) — exactly the i32 wrap point.
	mut u := cbor.new_unpacker(b('7a80000000'), cbor.DecodeOpts{})
	if _ := u.unpack_text() {
		assert false, 'expected EOF rejection at 2^31 boundary'
	}
}

fn test_skip_value_rejects_oversized_text_length() {
	mut u := cbor.new_unpacker(b('7bffffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject u64::max text length'
	}
}

fn test_skip_value_rejects_oversized_bytes_length() {
	mut u := cbor.new_unpacker(b('5bffffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject u64::max bytes length'
	}
}

fn test_skip_value_rejects_oversized_indef_chunk() {
	// 7f 7b ff..ff: indef text whose first chunk claims u64::max bytes.
	mut u := cbor.new_unpacker(b('7f7bffffffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject u64::max chunk length'
	}
}

// ---------------------------------------------------------------------
// Pre-allocation safety: the array/map header argument is a u64. The
// decoder must refuse any value it can't realistically allocate (cast
// overflow at i64::max, or item count > remaining bytes), instead of
// triggering a `negative .cap` panic or attempting a multi-GB malloc.
// ---------------------------------------------------------------------

fn test_value_decode_rejects_array_header_at_i64_max() {
	mut u := cbor.new_unpacker(b('9b7fffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_value() {
		assert false, 'unpack_value must reject implausible array length'
	}
}

fn test_value_decode_rejects_map_header_at_i64_max() {
	mut u := cbor.new_unpacker(b('bb7fffffffffffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_value() {
		assert false, 'unpack_value must reject implausible map length'
	}
}

fn test_value_decode_rejects_array_longer_than_buffer() {
	// 9a 00ffffff = array of ~16M items in a 5-byte payload.
	mut u := cbor.new_unpacker(b('9a00ffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_value() {
		assert false, 'unpack_value must reject array longer than buffer'
	}
}

fn test_value_decode_rejects_map_longer_than_buffer() {
	mut u := cbor.new_unpacker(b('ba00ffffff'), cbor.DecodeOpts{})
	if _ := u.unpack_value() {
		assert false, 'unpack_value must reject map longer than buffer'
	}
}

// ---------------------------------------------------------------------
// skip_value strictness: must reject every malformed shape that the
// typed unpack_* paths reject, otherwise malformed CBOR slips through
// RawMessage, Unmarshaler, and unknown-field skipping into downstream
// consumers.
// ---------------------------------------------------------------------

fn test_skip_value_rejects_simple_below_32() {
	// f8 1f = simple(1-byte form, value 31). RFC 8949 §3.3: values < 32
	// must use the inline form (info 0..23).
	mut u := cbor.new_unpacker(b('f81f'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject simple < 32 in 1-byte form'
	}
}

fn test_unpack_raw_rejects_simple_below_32() {
	mut u := cbor.new_unpacker(b('f81f'), cbor.DecodeOpts{})
	if _ := u.unpack_raw() {
		assert false, 'unpack_raw must reject simple < 32 in 1-byte form'
	}
}

// ---------------------------------------------------------------------
// deny_duplicate_keys covers every map decode path (typed map, struct,
// Value tree definite, Value tree indefinite). RFC 8949 §5.6: map keys
// "should be unique"; deny_duplicate_keys turns that into a hard error.
// ---------------------------------------------------------------------

fn test_value_decode_rejects_duplicate_keys_when_opted_in() {
	// a2 01 01 01 02 = {1: 1, 1: 2} — duplicate uint key 1.
	bytes := b('a201010102')
	if _ := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{ deny_duplicate_keys: true }) {
		assert false, 'expected duplicate-key rejection on Value path'
	}
}

fn test_value_decode_rejects_indef_map_duplicate_keys_when_opted_in() {
	// bf 01 01 01 02 ff = indef map with key 1 twice.
	bytes := b('bf01010102ff')
	if _ := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{ deny_duplicate_keys: true }) {
		assert false, 'expected duplicate-key rejection on indef Value map'
	}
}

fn test_value_decode_tolerates_duplicates_by_default() {
	// Default mode is permissive (matches Go encoding/cbor and QCBOR);
	// the option is opt-in.
	bytes := b('a201010102')
	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{}) or {
		assert false, 'default mode must accept duplicates: ${err}'
		return
	}
	assert v is cbor.Map
}

fn test_value_decode_detects_nan_keys_as_duplicate() {
	// bf f9 7e00 01 f9 7e00 02 ff = {NaN: 1, NaN: 2}, both half qNaN.
	// IEEE 754 says NaN != NaN, but RFC §5.6 dedup is over the encoded
	// bytes — two identical NaN encodings ARE equal.
	bytes := b('bff97e0001f97e0002ff')
	if _ := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{ deny_duplicate_keys: true }) {
		assert false, 'two NaN keys must be detected as duplicates'
	}
}

fn test_value_decode_keeps_distinct_float_widths_separate() {
	// Same numeric value, different wire widths → distinct data items
	// per RFC §5.6 (encoded form match).
	// bf f9 3c00 01 fa 3f800000 02 ff = {f16(1.0): 1, f32(1.0): 2}
	bytes := b('bff93c0001fa3f80000002ff')
	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{ deny_duplicate_keys: true })!
	assert v is cbor.Map
	if v is cbor.Map {
		assert v.pairs.len == 2
	}
}

// ---------------------------------------------------------------------
// Encoder indef state machine. RFC 8949 §3.2.3: an indef text/bytes
// string may only contain definite-length strings of the same major
// type. Opening any other indef container inside it is malformed.
// ---------------------------------------------------------------------

fn test_pack_text_indef_rejects_self_nesting() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_text_indef()!
	if _ := p.pack_text_indef() {
		assert false, 'nested indef text must be rejected'
	}
}

fn test_pack_bytes_indef_rejects_self_nesting() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bytes_indef()!
	if _ := p.pack_bytes_indef() {
		assert false, 'nested indef bytes must be rejected'
	}
}

fn test_pack_array_indef_rejected_inside_indef_text() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_text_indef()!
	if _ := p.pack_array_indef() {
		assert false, 'pack_array_indef inside pack_text_indef must be rejected'
	}
}

fn test_pack_map_indef_rejected_inside_indef_text() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_text_indef()!
	if _ := p.pack_map_indef() {
		assert false, 'pack_map_indef inside pack_text_indef must be rejected'
	}
}

fn test_pack_array_indef_rejected_inside_indef_bytes() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bytes_indef()!
	if _ := p.pack_array_indef() {
		assert false, 'pack_array_indef inside pack_bytes_indef must be rejected'
	}
}

fn test_pack_text_indef_rejected_inside_indef_bytes() {
	// A text-indef chunk inside a bytes-indef would carry the wrong
	// major type and break decode.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bytes_indef()!
	if _ := p.pack_text_indef() {
		assert false, 'pack_text_indef inside pack_bytes_indef must be rejected'
	}
}

fn test_pack_break_rejects_no_open_indef() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	if _ := p.pack_break() {
		assert false, 'break with no open indef context must be rejected'
	}
}

fn test_indef_array_inside_indef_array_allowed() {
	// Indef containers may freely nest; only indef text/bytes restrict
	// what their chunks may be (RFC 8949 §3.2.2 vs §3.2.3).
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_indef()!
	p.pack_array_indef()!
	p.pack_break()!
	p.pack_break()!
	assert p.bytes() == [u8(0x9f), 0x9f, 0xff, 0xff]
}

fn test_indef_text_round_trip() {
	// Sanity: a well-formed indef text (definite-length text chunks
	// followed by a break) round-trips through the decoder.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_text_indef()!
	p.pack_text('hel')
	p.pack_text('lo')
	p.pack_break()!
	mut u := cbor.new_unpacker(p.bytes().clone(), cbor.DecodeOpts{})
	s := u.unpack_text()!
	assert s == 'hello'
}

// ---------------------------------------------------------------------
// Encoder must refuse compositions that would silently corrupt the
// stream: a Tag with no content, a RawMessage with empty data, a
// Simple value in the reserved 24..31 range.
// ---------------------------------------------------------------------

fn test_pack_value_rejects_tag_with_empty_content() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	t := cbor.Tag{
		number:      42
		content_box: []
	}
	if _ := p.pack_value(t) {
		assert false, 'pack_value must reject Tag with empty content_box'
	}
}

fn test_new_tag_round_trips() {
	v := cbor.new_tag(99, cbor.new_uint(7))
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_value(v)!
	mut u := cbor.new_unpacker(p.bytes().clone(), cbor.DecodeOpts{})
	back := u.unpack_value()!
	assert back is cbor.Tag
	if back is cbor.Tag {
		assert back.number == 99
		c := back.content()
		assert c is cbor.IntNum
	}
}

fn test_pack_raw_rejects_empty_message() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	if _ := p.pack_raw(cbor.RawMessage{ data: [] }) {
		assert false, 'pack_raw must reject empty RawMessage'
	}
}

fn test_pack_value_rejects_reserved_simple() {
	// Simple values 24..31 are reserved per RFC 8949 §3.3; pack_simple
	// already rejects them, and pack_value must propagate rather than
	// emit zero bytes.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	if _ := p.pack_value(cbor.Simple{ value: 25 }) {
		assert false, 'reserved Simple must surface the pack_simple error'
	}
}

fn test_pack_simple_rejects_assigned_range() {
	// RFC 8949 §3.3 assigns 20..23 to false/true/null/undefined.
	// Encoding through pack_simple would silently produce wire-equivalent
	// bytes that decode back as Bool/Null/Undefined — surprising the
	// caller. The dedicated typed packers must be used instead.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	for v in [u8(20), 21, 22, 23] {
		if _ := p.pack_simple(v) {
			assert false, 'pack_simple(${v}) must be rejected'
		}
	}
	// 0..19 still inline cleanly, 32..255 still use the 1-byte form.
	p.pack_simple(0)!
	p.pack_simple(19)!
	p.pack_simple(32)!
	p.pack_simple(255)!
}

fn test_value_as_int_boundary() {
	// CBOR negative ints encode `-1 - magnitude`, so magnitude exactly
	// 2^63 maps to -2^63 - 1 — outside i64 range. Must return none, not
	// silently saturate to i64::min (which represents -2^63).
	bytes_overflow := b('3b8000000000000000') // negative magnitude=2^63
	v_over := cbor.decode[cbor.Value](bytes_overflow, cbor.DecodeOpts{})!
	assert v_over.as_int() == none, 'as_int must reject -2^63 - 1'

	// Just below the boundary: magnitude=2^63 - 1, value=i64::min, must fit.
	bytes_min := b('3b7fffffffffffffff')
	v_min := cbor.decode[cbor.Value](bytes_min, cbor.DecodeOpts{})!
	assert v_min.as_int()? == i64(-9223372036854775807 - 1)

	// Positive boundary: magnitude=i64::max fits, magnitude=i64::max+1 doesn't.
	bytes_pmax := b('1b7fffffffffffffff')
	v_pmax := cbor.decode[cbor.Value](bytes_pmax, cbor.DecodeOpts{})!
	assert v_pmax.as_int()? == i64(9223372036854775807)

	bytes_pover := b('1b8000000000000000')
	v_pover := cbor.decode[cbor.Value](bytes_pover, cbor.DecodeOpts{})!
	assert v_pover.as_int() == none, 'as_int must reject magnitudes > i64::max'
	// as_uint still gives the full unsigned range.
	assert v_pover.as_uint()? == u64(9223372036854775808)
}

fn test_struct_decode_rejects_duplicate_keys() {
	// Struct path: deny_duplicate_keys must fire on a repeated field
	// name, not silently let the second value overwrite the first.
	bytes := b('a26161016161 02'.replace(' ', ''))
	if _ := cbor.decode[Foo](bytes, cbor.DecodeOpts{ deny_duplicate_keys: true }) {
		assert false, 'struct decode must reject duplicate keys when opted in'
	}
}

struct Foo {
	a int
}

// ---------------------------------------------------------------------
// Canonical encoding (RFC 8949 §4.2.1) requires the preferred form
// (§4.2.2) regardless of the source's float width hint. A FloatNum
// carrying `.double` must shrink to half precision when the value
// fits, so re-encoding a Value tree in canonical mode is byte-stable.
// ---------------------------------------------------------------------

fn test_canonical_overrides_float_bits_hint() {
	v := cbor.FloatNum{
		value: 1.0
		bits:  .double
	}
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_value(v)!
	assert p.bytes() == [u8(0xf9), 0x3c, 0x00], 'got ${p.bytes().hex()}'
}

fn test_non_canonical_preserves_float_bits_hint() {
	// Without canonical mode the hint dictates the wire width, so the
	// encoder honours `.double` even when the value fits a half.
	v := cbor.FloatNum{
		value: 1.0
		bits:  .double
	}
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_value(v)!
	assert p.bytes()[0] == 0xfb, 'non-canonical must respect .double hint'
}

// ---------------------------------------------------------------------
// time.Time round-trip. Tag 1 (epoch) wraps an integer when the
// nanosecond component is zero, and a float for sub-second precision
// (RFC 8949 §3.4.2). The float-decode path uses math.round so values
// just below an integer second don't drop a nanosecond.
// ---------------------------------------------------------------------

fn test_time_round_trip_preserves_full_nanoseconds() {
	// Bit-exact round-trip across the full nanosecond range. The
	// sub-second path uses tag 0 (RFC 3339 ns string), so unlike a
	// tag-1 float it doesn't lose precision past the f64 mantissa.
	for ns in [int(1), 999, 1_000_000, 250_000_000, 999_999_999] {
		t := time.unix_nanosecond(1_700_000_000, ns)
		bytes := cbor.encode[time.Time](t, cbor.EncodeOpts{})!
		back := cbor.decode[time.Time](bytes, cbor.DecodeOpts{})!
		assert back.unix() == t.unix(), 'ns=${ns}: unix drift'
		assert back.nanosecond == ns, 'ns=${ns}: got ${back.nanosecond}'
	}
}

fn test_time_whole_seconds_use_int_tag() {
	// Whole-second values stay on the integer encoding (smaller wire,
	// canonical form).
	t := time.unix(1_700_000_000)
	bytes := cbor.encode[time.Time](t, cbor.EncodeOpts{})!
	mut u := cbor.new_unpacker(bytes, cbor.DecodeOpts{})
	v := u.unpack_value()!
	assert v is cbor.Tag
	if v is cbor.Tag {
		c := v.content()
		assert c is cbor.IntNum, 'whole-seconds path must stay integer'
	}
}

fn test_time_decode_rejects_nan_in_tag1_float() {
	// c1 fb 7ff8000000000001 = tag 1 + qNaN. Casting NaN to i64 is
	// undefined per C ABI (V's underlying), and NaN as a timestamp is
	// nonsense — must reject rather than silently decode to epoch 0.
	bytes := b('c1fb7ff8000000000001')
	if _ := cbor.decode[time.Time](bytes, cbor.DecodeOpts{}) {
		assert false, 'tag 1 NaN must be rejected'
	}
}

fn test_time_decode_rejects_inf_in_tag1_float() {
	bytes := b('c1fb7ff0000000000000') // +Inf
	if _ := cbor.decode[time.Time](bytes, cbor.DecodeOpts{}) {
		assert false, 'tag 1 +Inf must be rejected'
	}
}

fn test_time_decode_rejects_overflow_in_tag1_float() {
	// 1e30 — far beyond what i64 epoch seconds can hold; must error
	// rather than saturate silently.
	bytes := b('c1fb46293e5939a08cea')
	if _ := cbor.decode[time.Time](bytes, cbor.DecodeOpts{}) {
		assert false, 'tag 1 oversized float must be rejected'
	}
}

fn test_time_decode_rounds_nanoseconds() {
	// tag 1 + float64(1.999999999) — must round up to 1s 999_999_999ns
	// rather than truncating to 1s 999_999_998 (or worse, 0).
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(1)
	p.pack_float64(f64(1.999999999))
	bytes := p.bytes().clone()
	t := cbor.decode[time.Time](bytes, cbor.DecodeOpts{})!
	assert t.unix() == 1
	assert t.nanosecond >= 999_000_000, 'ns under-rounded: ${t.nanosecond}'
}

// ---------------------------------------------------------------------
// Opt-in UTF-8 validation at the encode boundary. The streaming
// pack_text trusts its input for performance, but encode[T] honours
// EncodeOpts.validate_utf8 so callers building strings from raw bytes
// can refuse to emit a payload the strict-by-default decoder would
// reject on the way back.
// ---------------------------------------------------------------------

fn test_encode_string_rejects_invalid_utf8_when_opted_in() {
	// Lone continuation byte 0x80 — invalid UTF-8.
	bad := unsafe { tos([u8(0x80)].data, 1) }
	if _ := cbor.encode[string](bad, cbor.EncodeOpts{ validate_utf8: true }) {
		assert false, 'expected validate_utf8 to reject invalid string'
	}
}

fn test_encode_string_passes_valid_utf8_with_validation() {
	got := cbor.encode[string]('héllo', cbor.EncodeOpts{ validate_utf8: true })!
	// `héllo` = 68 c3 a9 6c 6c 6f → header 66 (text len 6) + 6 bytes.
	assert got.len == 7
	assert got[0] == 0x66
}

fn test_encode_string_passes_invalid_utf8_when_validation_off() {
	// Default opts: encoder doesn't validate. The decoder will catch
	// the invalid sequence on decode (caller responsibility).
	bad := unsafe { tos([u8(0x80)].data, 1) }
	got := cbor.encode[string](bad, cbor.EncodeOpts{})!
	assert got.len == 2 // header 0x61 + 0x80
}

// ---------------------------------------------------------------------
// Sanity: well-formed pack_value calls (the common case) keep
// type-checking after the signature changes that the strictness
// tests above depend on.
// ---------------------------------------------------------------------

// ---------------------------------------------------------------------
// `decode[T]` rejects extra bytes after the top-level item by default
// so callers can't be tricked into accepting smuggled suffixes
// (concatenated items, leftover transport framing). Callers that
// genuinely want partial parsing opt in via allow_trailing_bytes.
// ---------------------------------------------------------------------

fn test_decode_rejects_trailing_bytes() {
	// `01 02` = uint(1) followed by uint(2). The second item must
	// surface as an error, not be silently dropped.
	bytes := b('0102')
	if _ := cbor.decode[u64](bytes, cbor.DecodeOpts{}) {
		assert false, 'expected trailing-byte rejection'
	}
}

fn test_decode_allows_trailing_bytes_when_opted_in() {
	bytes := b('0102')
	v := cbor.decode[u64](bytes, cbor.DecodeOpts{ allow_trailing_bytes: true })!
	assert v == 1, 'expected first item, got ${v}'
}

// ---------------------------------------------------------------------
// `decode_from` distinguishes io.Eof (legitimate end of stream) from
// transport errors. A truncated payload whose prefix happens to be a
// valid CBOR item must surface the underlying read failure rather than
// silently return a partial decode.
// ---------------------------------------------------------------------

struct FailingReader {
mut:
	emitted []u8
	pos     int
	fail_at int
}

fn (mut r FailingReader) read(mut buf []u8) !int {
	if r.pos >= r.fail_at {
		return error('FailingReader: simulated transport error')
	}
	if r.pos >= r.emitted.len {
		return io.Eof{}
	}
	n_max := if r.fail_at - r.pos < buf.len { r.fail_at - r.pos } else { buf.len }
	n := if r.emitted.len - r.pos < n_max { r.emitted.len - r.pos } else { n_max }
	for i in 0 .. n {
		buf[i] = r.emitted[r.pos + i]
	}
	r.pos += n
	return n
}

fn test_decode_from_propagates_reader_error() {
	// Emit `01 02` as the first chunk, then fail. Without the io.Eof
	// distinction the loop would `break` and decode `[1, 2]` as a
	// successful 2-item garbage; we want the read error to surface.
	mut r := FailingReader{
		emitted: [u8(0x01), 0x02]
		fail_at: 2
	}
	if _ := cbor.decode_from[u64](mut r, cbor.DecodeOpts{ max_stream_bytes: 1024 }) {
		assert false, 'expected reader error to propagate'
	}
}

struct EofReader {
mut:
	emitted []u8
	pos     int
}

fn (mut r EofReader) read(mut buf []u8) !int {
	if r.pos >= r.emitted.len {
		return io.Eof{}
	}
	n := if r.emitted.len - r.pos < buf.len { r.emitted.len - r.pos } else { buf.len }
	for i in 0 .. n {
		buf[i] = r.emitted[r.pos + i]
	}
	r.pos += n
	return n
}

fn test_decode_from_unbounded_propagates_reader_error() {
	// Same contract on the unbounded branch (no max_stream_bytes set):
	// transport errors must surface, not be swallowed as a clean EOF.
	mut r := FailingReader{
		emitted: [u8(0x01)]
		fail_at: 1
	}
	if v := cbor.decode_from[u64](mut r, cbor.DecodeOpts{}) {
		assert false, 'expected reader error to propagate, got ${v}'
	}
}

fn test_decode_from_treats_eof_as_normal() {
	// Same shape as above but the reader returns io.Eof cleanly after
	// emitting one valid item — must decode without error.
	mut r := EofReader{
		emitted: [u8(0x01)]
	}
	v := cbor.decode_from[u64](mut r, cbor.DecodeOpts{ max_stream_bytes: 1024 })!
	assert v == 1
}

// ---------------------------------------------------------------------
// User-defined `Marshaler` output is validated before splicing into
// the parent stream. Without this check, a buggy or hostile to_cbor()
// returning a truncated/malformed item silently corrupts the
// surrounding fields (the next field gets parsed as the bad item's
// claimed payload).
// ---------------------------------------------------------------------

struct BadMarshaler {}

pub fn (b BadMarshaler) to_cbor() []u8 {
	return [u8(0x78), 0x64] // text-string head claiming 100 bytes, no payload
}

struct EnvelopeWithBad {
	id    int
	bad   BadMarshaler
	other int
}

fn test_marshaler_output_validated() {
	e := EnvelopeWithBad{
		id:    1
		bad:   BadMarshaler{}
		other: 42
	}
	if _ := cbor.encode[EnvelopeWithBad](e, cbor.EncodeOpts{}) {
		assert false, 'malformed Marshaler output must be rejected'
	}
}

struct TrailingMarshaler {}

pub fn (t TrailingMarshaler) to_cbor() []u8 {
	return [u8(0x01), 0x02] // two valid items where one was promised
}

struct EnvelopeWithTrailing {
	id  int
	bad TrailingMarshaler
}

fn test_marshaler_rejects_trailing_bytes() {
	e := EnvelopeWithTrailing{
		id:  1
		bad: TrailingMarshaler{}
	}
	if _ := cbor.encode[EnvelopeWithTrailing](e, cbor.EncodeOpts{}) {
		assert false, 'Marshaler returning > 1 item must be rejected'
	}
}

// ---------------------------------------------------------------------
// `pack_to` refuses to ship a buffer with an open indef container.
// `bytes()` stays low-level and doesn't validate (callers can use
// `is_complete()` to check before reading the buffer directly).
// ---------------------------------------------------------------------

struct DropWriter {
mut:
	buf []u8
}

fn (mut w DropWriter) write(buf []u8) !int {
	w.buf << buf
	return buf.len
}

fn test_pack_to_rejects_open_indef() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_indef()!
	p.pack_int(1)
	mut w := DropWriter{}
	if _ := p.pack_to(mut w) {
		assert false, 'pack_to must reject buffer with open indef array'
	}
}

fn test_is_complete_reports_state() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	assert p.is_complete()
	p.pack_array_indef()!
	assert !p.is_complete()
	p.pack_int(1)
	p.pack_break()!
	assert p.is_complete()
}

// ---------------------------------------------------------------------
// Self-describe wrapping (RFC 8949 §3.4.6, tag 55799) is stripped
// transparently by `decode[T]` so payloads encoded with
// `EncodeOpts.self_describe` round-trip through the typed decoder.
// ---------------------------------------------------------------------

fn test_self_describe_round_trips_via_typed_decode() {
	bytes := cbor.encode[u64](42, cbor.EncodeOpts{ self_describe: true })!
	v := cbor.decode[u64](bytes, cbor.DecodeOpts{})!
	assert v == 42
}

fn test_self_describe_visible_via_unpacker() {
	// Callers that want to see the wrapper as a Tag can drive the
	// Unpacker directly — `unpack_value` does not strip the marker.
	bytes := cbor.encode[u64](42, cbor.EncodeOpts{ self_describe: true })!
	mut u := cbor.new_unpacker(bytes, cbor.DecodeOpts{})
	v := u.unpack_value()!
	if v is cbor.Tag {
		assert v.number == 55799
	} else {
		assert false, 'expected Tag(55799), got ${v.type_name()}'
	}
}

// ---------------------------------------------------------------------
// `unpack_uint` and `unpack_tag` roll back position on partial-arg
// failure, matching the contract of the other typed reads.
// ---------------------------------------------------------------------

fn test_unpack_uint_rollback_on_truncated_arg() {
	// 0x1b = uint(8-byte arg), no trailing payload — read_arg fails.
	mut u := cbor.new_unpacker([u8(0x1b)], cbor.DecodeOpts{ allow_trailing_bytes: true })
	if _ := u.unpack_uint() {
		assert false, 'expected truncated arg to error'
	}
	assert u.pos == 0, 'expected rollback to start, got pos=${u.pos}'
}

fn test_unpack_tag_rollback_on_truncated_arg() {
	mut u := cbor.new_unpacker([u8(0xdb)], cbor.DecodeOpts{ allow_trailing_bytes: true })
	if _ := u.unpack_tag() {
		assert false, 'expected truncated arg to error'
	}
	assert u.pos == 0, 'expected rollback to start, got pos=${u.pos}'
}

fn test_pack_value_well_formed() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_value(cbor.new_uint(1))!
	p.pack_value(cbor.new_text('x'))!
	assert p.bytes().len > 0
}

// ---------------------------------------------------------------------
// validate_utf8 must propagate into the canonical sub-encoders: the
// map / struct / Value-Map paths build temporary packers when sorting
// keys and used to drop the caller's option, silently letting invalid
// UTF-8 onto the wire even when strict-encode was requested.
// ---------------------------------------------------------------------

struct BadStrField {
	good string
	bad  string
}

fn test_validate_utf8_propagates_into_canonical_struct() {
	v := BadStrField{
		good: 'ok'
		bad:  unsafe { tos(c'\xff\xfe', 2) }
	}
	if _ := cbor.encode[BadStrField](v, cbor.EncodeOpts{
		canonical:     true
		validate_utf8: true
	})
	{
		assert false, 'canonical encode must reject invalid UTF-8 when validate_utf8 is set'
	}
}

fn test_validate_utf8_propagates_into_canonical_map() {
	mut m := map[string]string{}
	m['a'] = 'ok'
	m['b'] = unsafe { tos(c'\xff\xfe', 2) }
	if _ := cbor.encode[map[string]string](m, cbor.EncodeOpts{
		canonical:     true
		validate_utf8: true
	})
	{
		assert false, 'canonical encode must reject invalid UTF-8 in map values'
	}
}

fn test_validate_utf8_off_allows_invalid_in_canonical() {
	v := BadStrField{
		good: 'ok'
		bad:  unsafe { tos(c'\xff\xfe', 2) }
	}
	// Default opts (validate_utf8: false) must still let the bytes through
	// in canonical mode — the caller opted out of validation.
	bytes := cbor.encode[BadStrField](v, cbor.EncodeOpts{ canonical: true })!
	assert bytes.len > 0
}

// ---------------------------------------------------------------------
// SWAR ASCII fast path in `utf8_validate_slice` must remain correct on
// payloads of any length and any starting offset (the load is now via
// memcpy to be safe on strict-alignment targets — but the result must
// stay byte-for-byte identical with the per-byte path).
// ---------------------------------------------------------------------

fn test_validate_utf8_long_ascii_run() {
	// 257 bytes: spans multiple 8-byte SWAR chunks plus a non-multiple tail.
	s := 'a'.repeat(257)
	bytes := cbor.encode[string](s, cbor.EncodeOpts{ validate_utf8: true })!
	back := cbor.decode[string](bytes, cbor.DecodeOpts{ validate_utf8: true })!
	assert back == s
}

fn test_validate_utf8_non_ascii_after_swar_chunks() {
	// 16 ASCII bytes (two SWAR chunks) followed by a 2-byte UTF-8 rune,
	// then more ASCII. Confirms the fast path bails out cleanly into the
	// per-byte decoder when the high bit appears.
	s := 'aaaaaaaa' + 'aaaaaaaa' + 'é' + 'bbbb'
	bytes := cbor.encode[string](s, cbor.EncodeOpts{ validate_utf8: true })!
	back := cbor.decode[string](bytes, cbor.DecodeOpts{ validate_utf8: true })!
	assert back == s
}
