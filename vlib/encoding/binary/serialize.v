module binary

struct EncodeState {
mut:
	b []u8
	// pre-allocated buffers
	b2         []u8 = [u8(0), 0]
	b4         []u8 = [u8(0), 0, 0, 0]
	b8         []u8 = [u8(0), 0, 0, 0, 0, 0, 0, 0]
	big_endian bool
}

@[params]
pub struct EncodeConfig {
pub mut:
	buffer_len int = 1024
	big_endian bool // use big endian encoding the data
}

// encode_binary encode a T type data into u8 array.
// for encoding struct, you can use `@[serialize: '-']` to skip field.
pub fn encode_binary[T](obj T, config EncodeConfig) ![]u8 {
	mut s := EncodeState{
		b:          []u8{cap: config.buffer_len}
		big_endian: config.big_endian
	}
	$if T is $array {
		encode_array(mut s, obj)!
	} $else $if T is $string {
		encode_string(mut s, obj)!
	} $else $if T is $struct {
		encode_struct(mut s, obj)!
	} $else $if T is $map {
		encode_map(mut s, obj)!
	} $else {
		encode_primitive(mut s, obj)!
	}
	return s.b
}

fn encode_struct[T](mut s EncodeState, obj T) ! {
	$for field in T.fields {
		mut is_skip := false
		for attr in field.attrs {
			f := attr.split_any(':')
			if f.len == 2 {
				match f[0].trim_space() {
					'serialize' {
						// @[serialize:'-']
						if f[1].trim_space() == '-' {
							is_skip = true
						}
					}
					else {}
				}
			}
		}
		if !is_skip {
			value := obj.$(field.name)
			$if field.typ is $array {
				encode_array(mut s, value)!
			} $else $if field.typ is $string {
				encode_string(mut s, value)!
			} $else $if field.typ is $struct {
				encode_struct(mut s, value)!
			} $else $if field.typ is $map {
				encode_map(mut s, value)!
			} $else {
				encode_primitive(mut s, value)!
			}
		}
	}
}

// help unions for bypass `-cstrict`/ `-Wstrict-aliasing` check.
union U32_F32 {
	u u32
	f f32
}

union U64_F64 {
	u u64
	f f64
}

fn encode_primitive[T](mut s EncodeState, value T) ! {
	$if T is int {
		// NOTE: `int` always use 64bit
		s.put_u64(u64(value))
	} $else $if T is u8 {
		s.put_u8(u8(value))
	} $else $if T is u16 {
		s.put_u16(u16(value))
	} $else $if T is u32 {
		s.put_u32(u32(value))
	} $else $if T is u64 {
		s.put_u64(u64(value))
	} $else $if T is i8 {
		s.put_u8(u8(value))
	} $else $if T is i16 {
		s.put_u16(u16(value))
	} $else $if T is i32 {
		s.put_u32(u32(value))
	} $else $if T is i64 {
		s.put_u64(u64(value))
	} $else $if T is f32 {
		unsafe { s.put_u32(U32_F32{ f: value }.u) }
	} $else $if T is f64 {
		unsafe { s.put_u64(U64_F64{ f: value }.u) }
	} $else $if T is bool {
		s.put_u8(u8(value))
	} $else $if T is rune {
		s.put_u32(u32(value))
	} $else $if T is isize {
		if sizeof(isize) == 4 {
			s.put_u32(u32(value))
		} else {
			s.put_u64(u64(value))
		}
	} $else $if T is usize {
		if sizeof(usize) == 4 {
			s.put_u32(u32(value))
		} else {
			s.put_u64(u64(value))
		}
	} $else $if T is voidptr {
		s.put_u64(u64(value))
	} $else {
		// TODO: `any` type support?
		return error('${@FN}(): unsupported type ${typeof(value).name}')
	}
}

fn encode_array[T](mut s EncodeState, arr []T) ! {
	s.put_u64(u64(arr.len))

	$if T is u8 {
		// optimization for `[]u8`
		s.b << arr
	} $else {
		for element in arr {
			$if T is $string {
				encode_string(mut s, element)!
			} $else $if T is $struct {
				encode_struct(mut s, element)!
			} $else $if T is $map {
				encode_map(mut s, element)!
			} $else {
				encode_primitive(mut s, element)!
			}
		}
	}
}

fn encode_string(mut s EncodeState, str string) ! {
	s.put_u64(u64(str.len))
	s.b << str.bytes()
}

fn encode_map[K, V](mut s EncodeState, m map[K]V) ! {
	s.put_u64(u64(m.len))

	for k, v in m {
		// encode key first
		// Maps can have keys of type string, rune, integer, float or voidptr.
		$if K is $string {
			encode_string(mut s, k)!
		} $else {
			encode_primitive(mut s, k)!
		}

		// encode value
		$if V is $string {
			encode_string(mut s, v)!
		} $else $if V is $struct {
			encode_struct(mut s, v)!
		} $else $if V is $map {
			encode_map(mut s, v)!
		} $else {
			encode_primitive(mut s, v)!
		}
	}
}

struct DecodeState {
mut:
	b          []u8
	b2         []u8 = [u8(0), 0]
	b4         []u8 = [u8(0), 0, 0, 0]
	b8         []u8 = [u8(0), 0, 0, 0, 0, 0, 0, 0]
	offset     int
	big_endian bool
}

@[params]
pub struct DecodeConfig {
pub mut:
	buffer_len int = 1024
	big_endian bool // use big endian decode the data
}

// decode_binary decode a u8 array into T type data.
// for decoding struct, you can use `@[serialize: '-']` to skip field.
pub fn decode_binary[T](b []u8, config DecodeConfig) !T {
	mut s := DecodeState{
		b:          b
		big_endian: config.big_endian
	}
	$if T is $array {
		return decode_array(mut s, T{})!
	} $else $if T is $string {
		return decode_string(mut s)!
	} $else $if T is $struct {
		return decode_struct(mut s, T{})!
	} $else $if T is $map {
		return decode_map(mut s, T{})!
	} $else {
		return decode_primitive(mut s, unsafe { T(0) })!
	}
}

fn decode_struct[T](mut s DecodeState, _ T) !T {
	mut obj := T{}

	$for field in T.fields {
		mut is_skip := false
		for attr in field.attrs {
			f := attr.split_any(':')
			if f.len == 2 {
				match f[0].trim_space() {
					'serialize' {
						// @[serialize:'-']
						if f[1].trim_space() == '-' {
							is_skip = true
						}
					}
					else {}
				}
			}
		}
		if !is_skip {
			$if field.typ is $array {
				obj.$(field.name) = decode_array(mut s, obj.$(field.name))!
			} $else $if field.typ is $string {
				obj.$(field.name) = decode_string(mut s)!
			} $else $if field.typ is $struct {
				obj.$(field.name) = decode_struct(mut s, obj.$(field.name))!
			} $else $if field.typ is $map {
				obj.$(field.name) = decode_map(mut s, obj.$(field.name))!
			} $else {
				obj.$(field.name) = decode_primitive(mut s, obj.$(field.name))!
			}
		}
	}
	return obj
}

fn decode_primitive[T](mut s DecodeState, value T) !T {
	$if T is int {
		// NOTE: int always use 64bit
		return T(s.get_u64()!)
	} $else $if T is u8 {
		return T(s.get_u8()!)
	} $else $if T is u16 {
		return T(s.get_u16()!)
	} $else $if T is u32 {
		return T(s.get_u32()!)
	} $else $if T is u64 {
		return T(s.get_u64()!)
	} $else $if T is i8 {
		return T(s.get_u8()!)
	} $else $if T is i16 {
		return T(s.get_u16()!)
	} $else $if T is i32 {
		return T(s.get_u32()!)
	} $else $if T is i64 {
		return T(s.get_u64()!)
	} $else $if T is f32 {
		v := s.get_u32()!
		return unsafe {
			U32_F32{
				u: v
			}.f
		}
	} $else $if T is f64 {
		v := s.get_u64()!
		return unsafe {
			U64_F64{
				u: v
			}.f
		}
	} $else $if T is bool {
		return s.get_u8()! != 0
	} $else $if T is rune {
		return T(s.get_u32()!)
	} $else $if T is isize {
		if sizeof(isize) == 4 {
			return T(s.get_u32()!)
		} else {
			return T(s.get_u64()!)
		}
	} $else $if T is usize {
		if sizeof(usize) == 4 {
			return T(s.get_u32()!)
		} else {
			return T(s.get_u64()!)
		}
	} $else $if T is voidptr {
		return T(s.get_u64()!)
	} $else {
		// TODO: `any` type support?
		return error('${@FN}(): unsupported type ${typeof(value).name}')
	}
	return error('${@FN}(): impossible error')
}

fn decode_array[T](mut s DecodeState, _ []T) ![]T {
	len := int(s.get_u64()!)
	if len <= 0 || s.offset + len > s.b.len {
		return error('${@FN}(): invalid array length decode from stream')
	}
	mut arr := []T{cap: len}
	$if T is u8 {
		// optimization for `[]u8`
		arr << s.b[s.offset..s.offset + len]
		s.offset += len
	} $else {
		for _ in 0 .. len {
			if s.offset >= s.b.len {
				return error('${@FN}(): unexpected end of data')
			}
			$if T is $array {
				arr << decode_array(mut s, T{})!
			} $else $if T is $string {
				arr << decode_string(mut s)!
			} $else $if T is $struct {
				arr << decode_struct(mut s, T{})!
			} $else $if T is $map {
				arr << decode_map(mut s, T{})!
			} $else {
				arr << decode_primitive(mut s, unsafe { T(0) })!
			}
		}
	}
	return arr
}

fn decode_string(mut s DecodeState) !string {
	len := int(s.get_u64()!)
	if len <= 0 || s.offset + len > s.b.len {
		return error('${@FN}(): invalid string length decode from stream')
	}
	str := unsafe { s.b[s.offset..s.offset + len].bytestr() }
	s.offset += len
	return str
}

// `Any` is a sum type that lists the possible types to be decoded and used.
type Any = int
	| bool
	| f64
	| f32
	| i64
	| i32
	| i16
	| i8
	| map[string]Any
	| map[int]Any
	| string
	| u64
	| u32
	| u16
	| u8
	| rune
	| isize
	| usize
	| []Any

fn decode_map[K, V](mut s DecodeState, _ map[K]V) !map[K]V {
	len := int(s.get_u64()!)
	if len <= 0 || s.offset + len > s.b.len {
		return error('${@FN}(): invalid map length decode from stream')
	}

	mut m := map[K]V{}

	for _ in 0 .. len {
		// decode key first
		// Maps can have keys of type string, rune, integer, float or voidptr.
		mut k := Any(0)
		$if K is $string {
			k = decode_string(mut s)!
		} $else {
			k = decode_primitive(mut s, unsafe { K(0) })!
		}

		// decode value
		$if V is $struct {
			v := decode_struct(mut s, V{})!
			m[k as K] = v
		} $else $if V is $map {
			v := decode_map(mut s, V{})!
			m[k as K] = v
		} $else $if V is $string {
			v := decode_string(mut s)!
			m[k as K] = v
		} $else {
			v := decode_primitive(mut s, unsafe { V(0) })!
			m[k as K] = v
		}
	}
	return m
}

@[inline]
fn (mut s DecodeState) get_u64() !u64 {
	if s.offset + 8 > s.b.len {
		return error('${@FN}(): bytes length is not enough for u64')
	}
	defer {
		s.offset += 8
	}
	if s.big_endian {
		return big_endian_u64_at(s.b, s.offset)
	} else {
		return little_endian_u64_at(s.b, s.offset)
	}
}

@[inline]
fn (mut s DecodeState) get_u32() !u32 {
	if s.offset + 4 > s.b.len {
		return error('${@FN}(): bytes length is not enough for u32')
	}
	defer {
		s.offset += 4
	}
	if s.big_endian {
		return big_endian_u32_at(s.b, s.offset)
	} else {
		return little_endian_u32_at(s.b, s.offset)
	}
}

@[inline]
fn (mut s DecodeState) get_u16() !u16 {
	if s.offset + 2 > s.b.len {
		return error('${@FN}(): bytes length is not enough for u16')
	}
	defer {
		s.offset += 2
	}
	if s.big_endian {
		return big_endian_u16_at(s.b, s.offset)
	} else {
		return little_endian_u16_at(s.b, s.offset)
	}
}

@[inline]
fn (mut s DecodeState) get_u8() !u8 {
	if s.offset + 1 > s.b.len {
		return error('${@FN}(): bytes length is not enough for u8')
	}
	defer {
		s.offset += 1
	}
	return s.b[s.offset]
}

@[inline]
fn (mut s EncodeState) put_u64(value u64) {
	if s.big_endian {
		big_endian_put_u64(mut s.b8, value)
	} else {
		little_endian_put_u64(mut s.b8, value)
	}
	s.b << s.b8
}

@[inline]
fn (mut s EncodeState) put_u32(value u32) {
	if s.big_endian {
		big_endian_put_u32(mut s.b4, value)
	} else {
		little_endian_put_u32(mut s.b4, value)
	}
	s.b << s.b4
}

@[inline]
fn (mut s EncodeState) put_u16(value u16) {
	if s.big_endian {
		big_endian_put_u16(mut s.b2, value)
	} else {
		little_endian_put_u16(mut s.b2, value)
	}
	s.b << s.b2
}

@[inline]
fn (mut s EncodeState) put_u8(value u8) {
	s.b << value
}
