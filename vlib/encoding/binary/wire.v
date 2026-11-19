// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// Wire is a fixed-layout, padding-free binary encoding for structs carrying
// `@[wire: 'little_endian']` or `@[wire: 'big_endian']`. Field declaration
// order is the encode order and the declared byte order governs multi-byte
// values, so the same value encodes identically on every platform. There is no
// padding, no length prefix, and no version header: every wire struct has one
// stable byte representation.
//
// Supported field types are the fixed-width scalars u8/u16/u32/u64,
// i8/i16/i32/i64, bool (one byte, 0 or 1), f32/f64 (IEEE 754 bits), enums
// with an explicit fixed-size backing (`enum E as u8`, ...), fixed arrays of
// those (`[N]T`), and nested structs that are themselves wire structs. Each
// nested struct uses its own declared byte order. Anything else (int, uint,
// isize, usize, string, slices, maps, options, results, pointers, channels,
// functions, sum types, embeds) is rejected at compile time by the checker and
// by `$compile_error` fallbacks below.
//
// Decoding is bounds-checked: truncated input and trailing bytes both return
// errors.

// wire_byte_order returns the declared byte order of a wire struct (`'little_endian'`
// or `'big_endian'`), or '' when the struct carries no usable `@[wire: ...]` attribute.
fn wire_byte_order[T]() string {
	mut order := ''
	$for attr in T.attributes {
		if attr.name == 'wire' {
			order = normalize_attr_arg(attr.arg)
		}
	}
	return order
}

// wire_put_uint appends the low `size` bytes of `v` to `out` in the given byte order.
fn wire_put_uint(mut out []u8, v u64, size int, big bool) {
	if big {
		for i := size - 1; i >= 0; i-- {
			out << u8(v >> u64(i * 8))
		}
	} else {
		for i in 0 .. size {
			out << u8(v >> u64(i * 8))
		}
	}
}

// encode_wire encodes a wire struct to its stable byte representation.
// The struct must carry `@[wire: 'little_endian']` or `@[wire: 'big_endian']`;
// nested wire structs use their own declared byte order.
pub fn encode_wire[T](val T) []u8 {
	$if T is $struct {
		mut out := []u8{cap: wire_size_value(val)}
		wire_encode_struct(mut out, val)
		return out
	} $else {
		$compile_error('encoding.binary: encode_wire supports only struct types carrying @[wire]')
	}
}

// decode_wire decodes a wire struct from its stable byte representation.
// Truncated input and trailing bytes both return errors.
pub fn decode_wire[T](b []u8) !T {
	$if T is $struct {
		mut r := WireReader{
			b: b
		}
		mut val := T{}
		wire_decode_struct(mut r, mut val)!
		if r.off != b.len {
			return error('encoding.binary: wire decode trailing bytes (decoded ${r.off} of ${b.len} byte(s))')
		}
		return val
	} $else {
		$compile_error('encoding.binary: decode_wire supports only struct types carrying @[wire]')
	}
}

// wire_size returns the encoded length in bytes of a wire struct value.
// The size depends only on the type, never on the value.
pub fn wire_size[T](val T) int {
	return wire_size_value(val)
}

// wire_encode_struct appends the fields of a wire struct to `out` in
// declaration order, using the struct's own declared byte order.
fn wire_encode_struct[T](mut out []u8, val T) {
	order := wire_byte_order[T]()
	if order != 'little_endian' && order != 'big_endian' {
		panic("encoding.binary: encode_wire: struct `${T.name}` must carry @[wire: 'little_endian'] or @[wire: 'big_endian']")
	}
	big := order == 'big_endian'
	$for field in T.fields {
		wire_encode_value(mut out, val.$(field.name), big)
	}
}

// wire_decode_struct decodes every field of a wire struct in declaration
// order, using the struct's own declared byte order.
fn wire_decode_struct[T](mut r WireReader, mut val T) ! {
	order := wire_byte_order[T]()
	if order != 'little_endian' && order != 'big_endian' {
		return error("encoding.binary: decode_wire: struct `${T.name}` must carry @[wire: 'little_endian'] or @[wire: 'big_endian']")
	}
	big := order == 'big_endian'
	$for field in T.fields {
		val.$(field.name) = wire_decode_value(mut r, val.$(field.name), big)!
	}
}

// wire_encode_value appends one wire value to `out`.
fn wire_encode_value[T](mut out []u8, val T, big bool) {
	$if T is u8 {
		out << val
	} $else $if T is i8 {
		out << u8(val)
	} $else $if T is bool {
		out << u8(val)
	} $else $if T is u16 {
		wire_put_uint(mut out, u64(val), 2, big)
	} $else $if T is i16 {
		wire_put_uint(mut out, u64(val), 2, big)
	} $else $if T is u32 {
		wire_put_uint(mut out, u64(val), 4, big)
	} $else $if T is i32 {
		wire_put_uint(mut out, u64(val), 4, big)
	} $else $if T is u64 {
		wire_put_uint(mut out, val, 8, big)
	} $else $if T is i64 {
		wire_put_uint(mut out, u64(val), 8, big)
	} $else $if T is f32 {
		wire_put_uint(mut out, u64(unsafe { U32_F32{ f: val }.u }), 4, big)
	} $else $if T is f64 {
		wire_put_uint(mut out, unsafe { U64_F64{ f: val }.u }, 8, big)
	} $else $if T is $enum {
		wire_put_uint(mut out, u64(val), int(sizeof(T)), big)
	} $else $if T is $struct {
		wire_encode_struct(mut out, val)
	} $else $if T is $array_fixed {
		for i in 0 .. val.len {
			wire_encode_value(mut out, val[i], big)
		}
	} $else {
		$compile_error('encoding.binary: wire field has unsupported type (only u8/u16/u32/u64/i8/i16/i32/i64/bool/f32/f64, enums with explicit fixed-size backing, fixed arrays, and nested wire structs are supported)')
	}
}

// wire_decode_value decodes one wire value, advancing the reader.
fn wire_decode_value[T](mut r WireReader, zero T, big bool) !T {
	$if T is u8 {
		return u8(r.next(1, big)!)
	} $else $if T is i8 {
		return i8(r.next(1, big)!)
	} $else $if T is bool {
		return r.next(1, big)! != 0
	} $else $if T is u16 {
		return u16(r.next(2, big)!)
	} $else $if T is i16 {
		return i16(r.next(2, big)!)
	} $else $if T is u32 {
		return u32(r.next(4, big)!)
	} $else $if T is i32 {
		return i32(r.next(4, big)!)
	} $else $if T is u64 {
		return r.next(8, big)!
	} $else $if T is i64 {
		return i64(r.next(8, big)!)
	} $else $if T is f32 {
		return unsafe {
			U32_F32{
				u: u32(r.next(4, big)!)
			}.f
		}
	} $else $if T is f64 {
		return unsafe {
			U64_F64{
				u: r.next(8, big)!
			}.f
		}
	} $else $if T is $enum {
		raw := r.next(int(sizeof(T)), big)!
		return unsafe { T(raw) }
	} $else $if T is $struct {
		mut inner := zero
		wire_decode_struct(mut r, mut inner)!
		return inner
	} $else $if T is $array_fixed {
		mut arr := zero
		for i in 0 .. arr.len {
			arr[i] = wire_decode_value(mut r, arr[i], big)!
		}
		return arr
	} $else {
		$compile_error('encoding.binary: wire field has unsupported type (only u8/u16/u32/u64/i8/i16/i32/i64/bool/f32/f64, enums with explicit fixed-size backing, fixed arrays, and nested wire structs are supported)')
	}
}

// wire_size_value returns the encoded length in bytes of one wire value.
fn wire_size_value[T](val T) int {
	$if T is u8 || T is i8 || T is bool {
		return 1
	} $else $if T is u16 || T is i16 {
		return 2
	} $else $if T is u32 || T is i32 || T is f32 {
		return 4
	} $else $if T is u64 || T is i64 || T is f64 {
		return 8
	} $else $if T is $enum {
		return int(sizeof(T))
	} $else $if T is $struct {
		mut n := 0
		$for field in T.fields {
			n += wire_size_value(val.$(field.name))
		}
		return n
	} $else $if T is $array_fixed {
		if val.len == 0 {
			return 0
		}
		return val.len * wire_size_value(val[0])
	} $else {
		$compile_error('encoding.binary: wire_size: unsupported type (only u8/u16/u32/u64/i8/i16/i32/i64/bool/f32/f64, enums with explicit fixed-size backing, fixed arrays, and nested wire structs are supported)')
	}
}

// WireReader is a bounds-checked cursor over the input of wire_decode_value.
struct WireReader {
	b []u8
mut:
	off int
}

// need fails when `n` bytes cannot be read at the current offset.
fn (mut r WireReader) need(n int) ! {
	if r.off < 0 || r.off + n > r.b.len {
		return error('encoding.binary: wire decode out of range (need ${n} byte(s) at offset ${r.off}, got len ${r.b.len})')
	}
}

// next reads the next `n` bytes as an unsigned integer in the given byte order.
fn (mut r WireReader) next(n int, big bool) !u64 {
	r.need(n)!
	mut v := u64(0)
	if big {
		for i in 0 .. n {
			v = (v << 8) | u64(r.b[r.off + i])
		}
	} else {
		for i in 0 .. n {
			v |= u64(r.b[r.off + i]) << u64(i * 8)
		}
	}
	r.off += n
	return v
}
