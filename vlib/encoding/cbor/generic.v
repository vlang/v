module cbor

import math
import time

// Generic comptime-driven encoder/decoder. The pack[T] / unpack[T]
// methods below dispatch on T at compile time, so each call site
// monomorphises into straight-line code with no runtime type tests.
//
// Supported targets:
//   * bool, all signed/unsigned integer widths, f32, f64
//   * string (text), []u8 (byte string), enums (encoded as int)
//   * `$array` (any V array) and `$map` (any K with a primitive scalar
//     decoder — string, signed/unsigned ints, bool — plus any V).
//   * `$struct` (encoded as a string-keyed map; honours
//     `@[cbor: 'alt']`, `@[skip]`, `@[cbor: '-']`, optional fields)
//   * `time.Time` — whole seconds use tag 1 (epoch seconds, integer);
//     sub-second values use tag 0 (RFC 3339 string with nanosecond
//     precision). Decode accepts tag 0 (RFC 3339 text) or tag 1
//     (integer or float).
//   * `RawMessage`, `Value`, `Marshaler`/`Unmarshaler` implementers.

// pack encodes `val` into the packer's buffer using compile-time dispatch.
@[inline]
pub fn (mut p Packer) pack[T](val T) ! {
	$if T is RawMessage {
		p.pack_raw(val)!
	} $else $if T is Marshaler {
		bytes := val.to_cbor()
		if bytes.len == 0 {
			return error('cbor: ${T.name}.to_cbor() returned empty bytes')
		}
		// Validate the user's output is exactly one well-formed CBOR
		// item before splicing it into the parent stream. A malformed
		// or truncated Marshaler would otherwise silently corrupt the
		// surrounding fields (the next struct field would be parsed
		// from inside the bad item's claimed payload).
		mut probe := new_unpacker(bytes, DecodeOpts{})
		probe.skip_value() or {
			return error('cbor: ${T.name}.to_cbor() returned malformed CBOR: ${err.msg()}')
		}
		if !probe.done() {
			return error('cbor: ${T.name}.to_cbor() returned ${probe.remaining()} trailing byte(s) past one item')
		}
		p.reserve(bytes.len)
		unsafe { p.buf.push_many(bytes.data, bytes.len) }
	} $else $if T is Value {
		p.pack_value(val)!
	} $else $if T is time.Time {
		// Whole-second values use tag 1 (epoch seconds) + integer — the
		// most compact and canonical form (RFC 8949 §3.4.2). Sub-second
		// values fall back to tag 0 (RFC 3339 string) with nanosecond
		// precision: encoding the seconds.nanoseconds pair as a tag-1
		// float would lose ~µs of resolution past the year 2001 (f64
		// can't carry both a 10-digit unix epoch and 9 fractional digits).
		if val.nanosecond == 0 {
			p.pack_tag(tag_epoch)
			p.pack_int(val.unix())
		} else {
			p.pack_tag(tag_date_time)
			p.pack_text(format_rfc3339_nano(val))
		}
	} $else $if T is string {
		if p.opts.validate_utf8 && !utf8_validate_slice(val.bytes(), 0, val.len) {
			return error('cbor: validate_utf8 set, but string contains invalid UTF-8 (len=${val.len})')
		}
		p.pack_text(val)
	} $else $if T is bool {
		p.pack_bool(val)
	} $else $if T is i8 {
		p.pack_int(i64(val))
	} $else $if T is i16 {
		p.pack_int(i64(val))
	} $else $if T is int {
		p.pack_int(i64(val))
	} $else $if T is i32 {
		p.pack_int(i64(val))
	} $else $if T is i64 {
		p.pack_int(val)
	} $else $if T is u8 {
		p.pack_uint(u64(val))
	} $else $if T is u16 {
		p.pack_uint(u64(val))
	} $else $if T is u32 {
		p.pack_uint(u64(val))
	} $else $if T is u64 {
		p.pack_uint(val)
	} $else $if T is f32 {
		p.pack_float(f64(val))
	} $else $if T is f64 {
		p.pack_float(val)
	} $else $if T is $enum {
		p.pack_int(i64(val))
	} $else $if T is []u8 {
		p.pack_bytes(val)
	} $else $if T is $array {
		p.pack_array_header(u64(val.len))
		for item in val {
			p.pack(item)!
		}
	} $else $if T is $map {
		p.pack_map_header(u64(val.len))
		if p.opts.canonical && val.len > 1 {
			// Sub-encoders inherit `validate_utf8` so the strict-encode
			// guarantee survives canonical mode. `self_describe` and
			// `initial_cap` stay local — the wrapper belongs to the top-level
			// stream only, and 16 B is enough for almost every key/value pair.
			sub_opts := EncodeOpts{
				initial_cap:   16
				canonical:     true
				validate_utf8: p.opts.validate_utf8
			}
			mut encoded_keys := [][]u8{cap: val.len}
			mut encoded_vals := [][]u8{cap: val.len}
			for k, item in val {
				mut ksub := new_packer(sub_opts)
				ksub.pack(k)!
				encoded_keys << ksub.bytes().clone()
				mut vsub := new_packer(sub_opts)
				vsub.pack(item)!
				encoded_vals << vsub.bytes().clone()
			}
			for i in sort_canonical_indices(encoded_keys) {
				p.reserve(encoded_keys[i].len + encoded_vals[i].len)
				unsafe {
					p.buf.push_many(encoded_keys[i].data, encoded_keys[i].len)
					p.buf.push_many(encoded_vals[i].data, encoded_vals[i].len)
				}
			}
		} else {
			for k, item in val {
				p.pack(k)!
				p.pack(item)!
			}
		}
	} $else $if T is $struct {
		mut strategy := ''
		$for attr in T.attributes {
			if attr.name == 'cbor_rename_all' {
				strategy = attr.arg
			}
		}
		mut field_count := 0
		$for field in T.fields {
			if !cbor_field_skipped(field) {
				field_count++
			}
		}
		p.pack_map_header(u64(field_count))
		if p.opts.canonical && field_count > 1 {
			// RFC 8949 §4.2.1: deterministic encoding requires keys to
			// be ordered by their encoded byte form, not by struct
			// declaration. Encode each (key, value) pair to a sub-buffer,
			// sort, then splice — same shape as the $map branch above.
			// `validate_utf8` propagates so strict-encode callers don't
			// silently lose the guarantee in canonical mode.
			sub_opts := EncodeOpts{
				initial_cap:   16
				canonical:     true
				validate_utf8: p.opts.validate_utf8
			}
			mut encoded_keys := [][]u8{cap: field_count}
			mut encoded_vals := [][]u8{cap: field_count}
			$for field in T.fields {
				if !cbor_field_skipped(field) {
					key := cbor_field_explicit_key(field) or {
						if strategy != '' { cbor_rename(field.name, strategy) } else { field.name }
					}
					mut ksub := new_packer(sub_opts)
					ksub.pack_text(key)
					encoded_keys << ksub.bytes().clone()
					mut vsub := new_packer(sub_opts)
					$if field.typ is $option {
						if val.$(field.name) == none {
							vsub.pack_null()
						} else {
							vsub.pack(get_value_from_optional(val.$(field.name)))!
						}
					} $else {
						vsub.pack(val.$(field.name))!
					}
					encoded_vals << vsub.bytes().clone()
				}
			}
			for i in sort_canonical_indices(encoded_keys) {
				p.reserve(encoded_keys[i].len + encoded_vals[i].len)
				unsafe {
					p.buf.push_many(encoded_keys[i].data, encoded_keys[i].len)
					p.buf.push_many(encoded_vals[i].data, encoded_vals[i].len)
				}
			}
		} else {
			$for field in T.fields {
				if !cbor_field_skipped(field) {
					key := cbor_field_explicit_key(field) or {
						if strategy != '' { cbor_rename(field.name, strategy) } else { field.name }
					}
					p.pack_text(key)
					$if field.typ is $option {
						if val.$(field.name) == none {
							p.pack_null()
						} else {
							p.pack(get_value_from_optional(val.$(field.name)))!
						}
					} $else {
						p.pack(val.$(field.name))!
					}
				}
			}
		}
	} $else {
		p.pack_null()
	}
}

// get_value_from_optional unwraps an Option<T> known to be `Some`.
// Its signature exists solely so V's generic inferrer can pick up the
// inner T at the comptime call site.
fn get_value_from_optional[T](val ?T) T {
	return val or { T{} }
}

// unpack reads one CBOR value from the buffer and converts it to T.
@[inline]
pub fn (mut u Unpacker) unpack[T]() !T {
	$if T is RawMessage {
		return u.unpack_raw()!
	} $else $if T is Unmarshaler {
		start := u.pos
		u.skip_value()!
		mut v := T{}
		v.from_cbor(u.data[start..u.pos])!
		return v
	} $else $if T is Value {
		return u.unpack_value()!
	} $else $if T is time.Time {
		return u.unpack_time()!
	} $else $if T is string {
		return u.unpack_text()!
	} $else $if T is bool {
		// Accept null as false-equivalent? No — strict by default.
		return u.unpack_bool()!
	} $else $if T is i8 {
		v := u.unpack_int()!
		if v < -128 || v > 127 {
			return int_range(u.pos, 'i8', v.str())
		}
		return i8(v)
	} $else $if T is i16 {
		v := u.unpack_int()!
		if v < -32_768 || v > 32_767 {
			return int_range(u.pos, 'i16', v.str())
		}
		return i16(v)
	} $else $if T is int {
		v := u.unpack_int()!
		if v < -2_147_483_648 || v > 2_147_483_647 {
			return int_range(u.pos, 'int', v.str())
		}
		return int(v)
	} $else $if T is i32 {
		v := u.unpack_int()!
		if v < -2_147_483_648 || v > 2_147_483_647 {
			return int_range(u.pos, 'i32', v.str())
		}
		return i32(v)
	} $else $if T is i64 {
		return u.unpack_int()!
	} $else $if T is u8 {
		v := u.unpack_int()!
		if v < 0 || v > 255 {
			return int_range(u.pos, 'u8', v.str())
		}
		return u8(v)
	} $else $if T is u16 {
		v := u.unpack_int()!
		if v < 0 || v > 65_535 {
			return int_range(u.pos, 'u16', v.str())
		}
		return u16(v)
	} $else $if T is u32 {
		v := u.unpack_int()!
		if v < 0 || v > 4_294_967_295 {
			return int_range(u.pos, 'u32', v.str())
		}
		return u32(v)
	} $else $if T is u64 {
		neg, mag := u.unpack_int_full()!
		if neg {
			return int_range(u.pos, 'u64', '-1 - ${mag}')
		}
		return mag
	} $else $if T is f32 {
		return f32(u.unpack_float()!)
	} $else $if T is f64 {
		return u.unpack_float()!
	} $else $if T is $enum {
		v := int(u.unpack_int()!)
		return unsafe { T(v) }
	} $else $if T is []u8 {
		return u.unpack_bytes()!
	} $else $if T is $array {
		mut out := T{}
		u.unpack_array_into(mut out)!
		return out
	} $else $if T is $map {
		mut out := T{}
		read_pairs_into_helper(mut u, mut out)!
		return out
	} $else $if T is $struct {
		mut result := T{}
		u.unpack_struct_into(mut result)!
		return result
	} $else {
		return error('cbor: unsupported target type')
	}
}

fn (mut u Unpacker) unpack_array_into[E](mut out []E) ! {
	hdr := u.unpack_array_header()!
	if hdr < 0 {
		// Indefinite.
		for {
			if u.consume_break() {
				break
			}
			out << u.unpack[E]()!
		}
		return
	}
	for _ in 0 .. hdr {
		out << u.unpack[E]()!
	}
}

// read_pairs_into_helper is a standalone (non-method) generic function;
// V's generic-method dispatch can drop the second type parameter when
// invoked from a comptime $map branch, while the standalone form
// monomorphises correctly.
fn read_pairs_into_helper[K, V](mut u Unpacker, mut out map[K]V) ! {
	hdr := u.unpack_map_header()!
	if hdr < 0 {
		for {
			if u.consume_break() {
				break
			}
			key := u.unpack[K]()!
			val := u.unpack[V]()!
			if u.opts.deny_duplicate_keys && key in out {
				return malformed(u.pos, 'duplicate map key')
			}
			out[key] = val
		}
		return
	}
	for _ in 0 .. hdr {
		key := u.unpack[K]()!
		val := u.unpack[V]()!
		if u.opts.deny_duplicate_keys && key in out {
			return malformed(u.pos, 'duplicate map key')
		}
		out[key] = val
	}
}

fn (mut u Unpacker) unpack_struct_into[T](mut result T) ! {
	mut strategy := ''
	$for attr in T.attributes {
		if attr.name == 'cbor_rename_all' {
			strategy = attr.arg
		}
	}
	hdr := u.unpack_map_header()!
	indef := hdr < 0
	mut remaining := if indef { i64(-1) } else { hdr }
	// Tracks keys already seen so deny_duplicate_keys can fire on struct
	// decode too (the typed-map and Value paths track separately). Built
	// only when the option is set, so the common case stays allocation-free.
	// O(1) lookup via V map keeps decode linear even on adversarial inputs
	// with thousands of distinct keys.
	mut seen_keys := map[string]bool{}
	for {
		if indef {
			if u.consume_break() {
				break
			}
		} else {
			if remaining == 0 {
				break
			}
			remaining--
		}
		key_ptr, key_len := u.read_text_view()!
		if u.opts.deny_duplicate_keys {
			key_str := unsafe { tos(key_ptr, key_len) }.clone()
			if key_str in seen_keys {
				return malformed(u.pos, 'duplicate map key "${key_str}"')
			}
			seen_keys[key_str] = true
		}
		mut matched := false
		$for field in T.fields {
			if !cbor_field_skipped(field) {
				name := cbor_field_explicit_key(field) or {
					if strategy != '' { cbor_rename(field.name, strategy) } else { field.name }
				}
				if !matched && key_len == name.len
					&& unsafe { C.memcmp(key_ptr, name.str, key_len) } == 0 {
					matched = true
					$if field.typ is $option {
						if u.pos < u.data.len && u.data[u.pos] == 0xf6 {
							u.pos++
							result.$(field.name) = none
						} else {
							mut inner := create_value_from_optional(result.$(field.name))
							u.unpack_into(mut inner)!
							result.$(field.name) = inner
						}
					} $else {
						u.unpack_into(mut result.$(field.name))!
					}
				}
			}
		}
		if !matched {
			start := u.pos
			u.skip_value()!
			if u.opts.deny_unknown_fields {
				return UnknownFieldError{
					pos:  start
					name: unsafe { tos(key_ptr, key_len) }
				}
			}
		}
	}
}

// read_text_view returns a (ptr, len) view into the underlying buffer
// for one definite-length text string. Avoids allocation when matching
// struct field names. Errors on indefinite-length text since we'd have
// to copy chunks anyway.
@[direct_array_access]
fn (mut u Unpacker) read_text_view() !(&u8, int) {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 3 {
		u.pos = start
		return type_mismatch(start, 'text', b)
	}
	info := b & 0x1f
	if info == 31 {
		u.pos = start
		return error('cbor: indefinite-length text not supported as map key (decoder)')
	}
	size := u.read_arg(info)!
	if size > u64(u.data.len - u.pos) {
		return eof_oversized(u.pos, size, u.data.len - u.pos)
	}
	size_int := int(size)
	if u.opts.validate_utf8 {
		if !u.is_utf8_at(u.pos, size_int) {
			return InvalidUtf8Error{
				pos: u.pos
			}
		}
	}
	ptr := unsafe { &u8(u.data.data) + u.pos }
	u.pos += size_int
	return ptr, size_int
}

@[direct_array_access; inline]
fn (u &Unpacker) is_utf8_at(start int, size int) bool {
	if size == 0 {
		return true
	}
	return utf8_validate_slice(u.data, start, size)
}

// utf8_validate_slice runs the standard UTF-8 validator on a slice
// without making an intermediate copy. Mirrors the FSM used by
// `vlib/encoding/utf8/utf8_util.v`. The 8-byte SWAR pre-scan turns a
// pure-ASCII payload (the common case: JSON-shaped keys, identifiers)
// into one load + one mask + one branch per 8 bytes.
@[direct_array_access]
fn utf8_validate_slice(data []u8, start int, size int) bool {
	mut i := start
	end := start + size
	for i < end {
		// 8-byte SWAR ASCII fast path: a pure-ASCII run skips the
		// per-byte FSM entirely. Triggers on every iteration so a single
		// non-ASCII rune doesn't disable the fast path for the rest.
		// `memcpy` into a stack u64 instead of `*(&u64(&data[i]))`: the
		// latter is undefined behaviour when `i` isn't 8-byte aligned, and
		// crashes on strict-alignment targets (e.g. some ARMv7, MIPS).
		// Modern C compilers lower this memcpy to a single unaligned load
		// on x86 / arm64, so the SWAR speed-up is preserved.
		for i + 8 <= end {
			mut chunk := u64(0)
			unsafe { C.memcpy(&chunk, &data[i], 8) }
			if chunk & 0x8080808080808080 != 0 {
				break
			}
			i += 8
		}
		if i >= end {
			break
		}
		c := data[i]
		if c < 0x80 {
			i++
			continue
		}
		mut n := 0
		if c & 0xe0 == 0xc0 {
			n = 2
		} else if c & 0xf0 == 0xe0 {
			n = 3
		} else if c & 0xf8 == 0xf0 {
			n = 4
		} else {
			return false
		}
		if i + n > end {
			return false
		}
		// Reject overlongs / surrogates / out-of-range.
		match n {
			2 {
				if c < 0xc2 {
					return false
				}
			}
			3 {
				b := data[i + 1]
				if c == 0xe0 && b < 0xa0 {
					return false
				}
				if c == 0xed && b > 0x9f {
					return false
				}
			}
			4 {
				b := data[i + 1]
				if c == 0xf0 && b < 0x90 {
					return false
				}
				if c == 0xf4 && b > 0x8f {
					return false
				}
				if c > 0xf4 {
					return false
				}
			}
			else {}
		}

		for k in 1 .. n {
			if data[i + k] & 0xc0 != 0x80 {
				return false
			}
		}
		i += n
	}
	return true
}

// create_value_from_optional returns a zero value of an Option's inner T.
// Exists so the comptime call site can infer T from a struct field.
fn create_value_from_optional[T](_val ?T) T {
	return T{}
}

// unpack_into fills the target through a mutable reference. The mut
// parameter exists so V's generic inferer picks up T from the
// `u.unpack_into(mut result.$(field.name))!` call site.
@[inline]
fn (mut u Unpacker) unpack_into[T](mut out T) ! {
	_ = out // vet's "unused parameter" check doesn't track write-only mut args
	out = u.unpack[T]()!
}

// format_rfc3339_nano emits a time.Time as RFC 3339 with full nanosecond
// precision ("YYYY-MM-DDTHH:mm:ss.nnnnnnnnnZ"). vlib's `time` module
// only goes down to milliseconds (`format_rfc3339`), but tag 0
// round-trips need 9 digits to preserve `time.Time.nanosecond` exactly.
// Inputs are normalised to UTC first so a `time.now()` from a local
// session is encoded as the correct instant rather than as wall-clock
// digits without an offset.
fn format_rfc3339_nano(t time.Time) string {
	utc := if t.is_local { t.local_to_utc() } else { t }
	return '${utc.year:04d}-${utc.month:02d}-${utc.day:02d}T${utc.hour:02d}:${utc.minute:02d}:${utc.second:02d}.${utc.nanosecond:09d}Z'
}

// --------------------------------------------------------------------
// time.Time decoding
// --------------------------------------------------------------------

fn (mut u Unpacker) unpack_time() !time.Time {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 6 {
		u.pos = start
		return type_mismatch(start, 'time tag', b)
	}
	number := u.read_arg(b & 0x1f)!
	match number {
		0 {
			s := u.unpack_text()!
			return time.parse_iso8601(s) or {
				return malformed(start, 'invalid RFC 3339 timestamp: ${err}')
			}
		}
		1 {
			peek := u.peek_byte() or { return error('cbor: missing tag-1 content') }
			major2 := peek >> 5
			if major2 == 0 || major2 == 1 {
				secs := u.unpack_int()!
				return time.unix(secs)
			}
			f := u.unpack_float()!
			// Reject NaN, ±Inf, and any magnitude that won't fit i64
			// before casting. Without this, NaN silently saturates to 0
			// (epoch 1970-01-01) and overflow saturates to i64::max,
			// either of which could bypass an application-level expiry
			// or freshness check.
			if math.is_nan(f) || math.is_inf(f, 0) {
				return malformed(start, 'tag 1 float must be finite, got ${f}')
			}
			if f >= 9_223_372_036_854_775_808.0 || f < -9_223_372_036_854_775_808.0 {
				return malformed(start, 'tag 1 float ${f} out of range for i64 epoch seconds')
			}
			whole := i64(math.floor(f))
			frac := f - f64(whole)
			// math.round (not i64-truncate) so 0.999_999_999s doesn't
			// silently round to 0 ns. Clamp to the valid ns range; the
			// only way to land on the boundary now is true rounding noise.
			mut ns := i64(math.round(frac * 1_000_000_000.0))
			if ns < 0 {
				ns = 0
			} else if ns > 999_999_999 {
				ns = 999_999_999
			}
			return time.unix_nanosecond(whole, int(ns))
		}
		else {
			u.pos = start
			return malformed(start, 'unexpected tag ${number} for time.Time')
		}
	}
}

// --------------------------------------------------------------------
// Struct attribute helpers
// --------------------------------------------------------------------

@[inline]
fn cbor_field_skipped[F](field F) bool {
	for attr in field.attrs {
		if attr == 'skip' {
			return true
		}
		if attr.starts_with('cbor:') {
			if val := parse_cbor_attr(attr) {
				if val == '-' {
					return true
				}
			}
		}
	}
	return false
}

// cbor_field_explicit_key returns the rename target from `@[cbor: '...']`
// when one is set, or `none` if the field has no explicit override.
// `@[cbor: '-']` and the empty form `@[cbor: '']` are treated as no
// override (skipping is handled by `cbor_field_skipped`).
@[inline]
fn cbor_field_explicit_key[F](field F) ?string {
	for attr in field.attrs {
		if attr.starts_with('cbor:') {
			if val := parse_cbor_attr(attr) {
				if val != '-' && val != '' {
					return val
				}
			}
		}
	}
	return none
}

fn cbor_rename(name string, strategy string) string {
	match strategy {
		'snake_case' { return cbor_to_snake(name) }
		'camelCase' { return cbor_to_camel(name) }
		'PascalCase' { return cbor_to_pascal(name) }
		'kebab-case' { return cbor_to_kebab(name) }
		'SCREAMING_SNAKE_CASE' { return cbor_to_snake(name).to_upper() }
		else { return name }
	}
}

fn cbor_to_snake(s string) string {
	mut out := []u8{cap: s.len + 4}
	for i, c in s {
		if c >= `A` && c <= `Z` {
			if i > 0 {
				out << `_`
			}
			out << u8(c + 32)
		} else {
			out << c
		}
	}
	return out.bytestr()
}

fn cbor_to_camel(s string) string {
	mut out := []u8{cap: s.len}
	mut upper_next := false
	for i, c in s {
		if c == `_` {
			upper_next = true
			continue
		}
		if upper_next && c >= `a` && c <= `z` {
			out << u8(c - 32)
			upper_next = false
		} else if i == 0 && c >= `A` && c <= `Z` {
			out << u8(c + 32)
		} else {
			out << c
		}
	}
	return out.bytestr()
}

fn cbor_to_pascal(s string) string {
	camel := cbor_to_camel(s)
	if camel.len == 0 {
		return camel
	}
	first := camel[0]
	if first >= `a` && first <= `z` {
		return u8(first - 32).ascii_str() + camel[1..]
	}
	return camel
}

fn cbor_to_kebab(s string) string {
	mut out := []u8{cap: s.len + 4}
	for i, c in s {
		if c >= `A` && c <= `Z` {
			if i > 0 {
				out << `-`
			}
			out << u8(c + 32)
		} else if c == `_` {
			out << `-`
		} else {
			out << c
		}
	}
	return out.bytestr()
}

fn parse_cbor_attr(attr string) ?string {
	idx := attr.index(':') or { return none }
	mut v := attr[idx + 1..].trim_space()
	if v.len >= 2 && ((v.starts_with("'") && v.ends_with("'"))
		|| (v.starts_with('"') && v.ends_with('"'))) {
		v = v[1..v.len - 1]
	}
	return v
}
