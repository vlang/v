module main

import encoding.binary
import math

fn main() {
	value := Vector3D{
		x: 1.0
		y: 2.0
		z: 3.0
		v: 'bob'
	}
	mut buf := []u8{len: 0, cap: 12}
	serialize_to(value, mut buf)!
	println(buf)
}

type Primitive = f64 | f32 | rune | i32 | u32 | i16 | u16 | i8 | u8 | bool

struct Vector3D {
	x f32
	y f32
	z f32
	v string
}

fn serialize_to[T](val T, mut output []u8) ! {
	$for v in Primitive.variants {
		$if v.typ is T {
			output << binary.encode_binary(val, binary.EncodeConfig{
				buffer_len: int(sizeof[T]())
				big_endian: false
			})!
			return
		}
	}
	$if T is string {
		if val.len > math.maxof[u16]() {
			return error('String too long to serialize')
		}
		bytes := val.bytes()
		output << binary.encode_binary(u16(bytes.len), binary.EncodeConfig{
			buffer_len: int(sizeof[u16]())
			big_endian: false
		})!
		output << bytes
		return
	}
	$for field in T.fields {
		serialize_to(val.$(field.name), mut output)!
	}
}
