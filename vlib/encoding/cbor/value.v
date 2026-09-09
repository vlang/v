module cbor

// Value is the dynamic representation of any CBOR data item. Use it when
// the schema isn't known at compile time, when you need to inspect tags,
// or when keys aren't strings:
//
//   v := cbor.decode[cbor.Value](bytes)!
//   match v {
//       cbor.Text { println(v.value) }
//       else { ... }
//   }
//
// For known schemas prefer `decode[YourStruct]` — it's faster and avoids
// the heap allocations of building a Value tree.
pub type Value = Array
	| Bool
	| Bytes
	| FloatNum
	| IntNum
	| Map
	| Null
	| Simple
	| Tag
	| Text
	| Undefined

// IntNum holds the full unsigned/negative CBOR integer range. CBOR allows
// values from -(2^64) to 2^64-1, which exceeds either i64 or u64 alone, so
// the sign bit is split out and the magnitude carried as u64.
//
//   for unsigned: negative=false, magnitude=value
//   for negative: negative=true,  magnitude=encoded_argument
//                 actual integer  = -1 - i64(magnitude) (when it fits i64)
pub struct IntNum {
pub:
	negative  bool
	magnitude u64
}

// Bytes is a CBOR byte string (major type 2).
pub struct Bytes {
pub mut:
	data []u8
}

// Text is a CBOR text string (major type 3, valid UTF-8).
pub struct Text {
pub:
	value string
}

// Array holds the elements of a CBOR array (major type 4).
pub struct Array {
pub mut:
	elements []Value
}

// MapPair represents one key/value entry in a CBOR map. CBOR allows any
// data item as a key, so we keep an ordered list of pairs rather than
// using V's `map[K]V`.
pub struct MapPair {
pub:
	key   Value
	value Value
}

// Map holds the ordered key/value pairs of a CBOR map (major type 5).
pub struct Map {
pub mut:
	pairs []MapPair
}

// Tag wraps a tagged data item (major type 6). The content is stored in
// a one-element slice rather than as a `&Value` reference: V can box and
// recurse a sumtype through a slice, while a direct `&Value` field
// requires manual heap allocation. Use `tag.content()` to access it.
pub struct Tag {
pub:
	number      u64
	content_box []Value
}

// content returns the Value enclosed by a Tag, or `Null{}` if missing.
@[inline]
pub fn (t &Tag) content() Value {
	if t.content_box.len > 0 {
		return t.content_box[0]
	}
	return Null{}
}

// Bool is the wrapped form of CBOR true/false (simple values 20/21).
pub struct Bool {
pub:
	value bool
}

// Null is the wrapped form of CBOR null (simple value 22).
pub struct Null {}

// Undefined is the wrapped form of CBOR undefined (simple value 23).
pub struct Undefined {}

// FloatBits records which IEEE 754 width the float was originally encoded
// at. The encoder honours this when re-emitting a Value, so round-tripping
// preserves the original byte width.
pub enum FloatBits as u8 {
	@none  = 0
	half   = 16
	single = 32
	double = 64
}

// FloatNum is a CBOR floating-point value (major type 7, additional info
// 25/26/27). `bits` records the wire width for fidelity on re-encoding;
// the default `.@none` lets the encoder pick the shortest IEEE 754 width
// that preserves the value (RFC 8949 §4.2.2 preferred serialisation), so
// hand-built FloatNum literals don't accidentally lock in 8-byte output.
pub struct FloatNum {
pub:
	value f64
	bits  FloatBits = .@none
}

// Simple is the catch-all for major type 7 simple values 0..255 not
// otherwise covered by Bool/Null/Undefined.
pub struct Simple {
pub:
	value u8
}

// new_uint wraps a u64 in a Value (unsigned-int variant).
@[inline]
pub fn new_uint(n u64) Value {
	return IntNum{
		negative:  false
		magnitude: n
	}
}

// new_int wraps a signed i64 in a Value, picking unsigned vs negative.
@[inline]
pub fn new_int(n i64) Value {
	if n >= 0 {
		return IntNum{
			negative:  false
			magnitude: u64(n)
		}
	}
	return IntNum{
		negative:  true
		magnitude: u64(-(n + 1))
	}
}

// new_negative wraps the encoded argument of a major-type-1 value, where
// the actual integer is -1 - magnitude. Useful when magnitude exceeds i64.
@[inline]
pub fn new_negative(magnitude u64) Value {
	return IntNum{
		negative:  true
		magnitude: magnitude
	}
}

// new_text wraps a string as a CBOR text Value.
@[inline]
pub fn new_text(s string) Value {
	return Text{
		value: s
	}
}

// new_bytes wraps a []u8 as a CBOR byte-string Value.
@[inline]
pub fn new_bytes(b []u8) Value {
	return Bytes{
		data: b
	}
}

// new_float wraps an f64 as a CBOR FloatNum that re-encodes at full
// precision unless `f64_to_half` / f32 conversion is lossless.
@[inline]
pub fn new_float(v f64) Value {
	return FloatNum{
		value: v
		bits:  .@none
	}
}

// new_tag wraps an existing Value with a tag number.
@[inline]
pub fn new_tag(number u64, content Value) Value {
	return Tag{
		number:      number
		content_box: [content]
	}
}

// is_nil returns true if v is the CBOR `null` value.
@[inline]
pub fn (v &Value) is_nil() bool {
	return v is Null
}

// is_undefined returns true if v is the CBOR `undefined` value.
@[inline]
pub fn (v &Value) is_undefined() bool {
	return v is Undefined
}

// as_int returns the value as an i64 when it fits, or none otherwise.
// Returns none for FloatNum, Text, etc.
//
// CBOR negative integers represent `-1 - magnitude`, so magnitude
// `2^63 - 1` maps to i64::min and magnitude `2^63` would map to
// `-2^63 - 1` — outside i64 range — hence the strict ">" cutoff for
// negatives. Use `as_uint` plus the `negative` flag to recover the
// full -2^64..2^64-1 CBOR range.
pub fn (v &Value) as_int() ?i64 {
	if v is IntNum {
		if v.negative {
			if v.magnitude > u64(max_i64) {
				return none
			}
			return -1 - i64(v.magnitude)
		}
		if v.magnitude > u64(max_i64) {
			return none
		}
		return i64(v.magnitude)
	}
	return none
}

// as_uint returns the value as a u64 if it's a non-negative integer, else none.
pub fn (v &Value) as_uint() ?u64 {
	if v is IntNum {
		if v.negative {
			return none
		}
		return v.magnitude
	}
	return none
}

// as_float returns the f64 value, or none if v isn't a FloatNum.
pub fn (v &Value) as_float() ?f64 {
	if v is FloatNum {
		return v.value
	}
	return none
}

// as_bool returns the boolean value, or none if v isn't a Bool.
pub fn (v &Value) as_bool() ?bool {
	if v is Bool {
		return v.value
	}
	return none
}

// as_string returns the text-string value, or none if v isn't Text.
pub fn (v &Value) as_string() ?string {
	if v is Text {
		return v.value
	}
	return none
}

// as_bytes returns the byte-string payload, or none if v isn't Bytes.
pub fn (v &Value) as_bytes() ?[]u8 {
	if v is Bytes {
		return v.data
	}
	return none
}

// as_array returns the elements of an Array, or none.
pub fn (v &Value) as_array() ?[]Value {
	if v is Array {
		return v.elements
	}
	return none
}

// as_map returns the pairs of a Map, or none.
pub fn (v &Value) as_map() ?[]MapPair {
	if v is Map {
		return v.pairs
	}
	return none
}

// as_tag returns (number, content) of a Tag, or none.
pub fn (v &Value) as_tag() ?(u64, Value) {
	if v is Tag {
		return v.number, v.content()
	}
	return none
}

// get does a linear lookup of a string-keyed entry in a Map.
// O(n) — for hot paths decode into a typed struct or `map[string]V`.
pub fn (v &Value) get(key string) ?Value {
	if v is Map {
		for pair in v.pairs {
			if pair.key is Text {
				if pair.key.value == key {
					return pair.value
				}
			}
		}
	}
	return none
}

// at returns the element at `index` of an Array.
pub fn (v &Value) at(index int) ?Value {
	if v is Array {
		if index >= 0 && index < v.elements.len {
			return v.elements[index]
		}
	}
	return none
}

// len returns the length of an Array, Map, Text, or Bytes value, or 0.
pub fn (v &Value) len() int {
	match v {
		Array { return v.elements.len }
		Map { return v.pairs.len }
		Text { return v.value.len }
		Bytes { return v.data.len }
		else { return 0 }
	}
}
