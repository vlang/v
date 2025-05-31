import runtime { is_big_endian }

union F32A {
	float f32
	value [4]u8
}

union F64A {
	float f64
	value [8]u8
}

pub fn read_f32le(bytes [4]u8) f32 {
	if is_big_endian() {
		bytes_rev := [4]u8{init: bytes[3 - index]}
		float := F32A{
			value: bytes_rev
		}
		unsafe {
			return float.float
		}
	} else {
		float := F32A{
			value: bytes
		}
		unsafe {
			return float.float
		}
	}
}

pub fn write_f32le(float f32) [4]u8 {
	float_copy := F32A{
		float: float
	}
	if is_big_endian() {
		unsafe {
			return [4]u8{init: float_copy.value[3 - index]}
		}
	} else {
		unsafe {
			return float_copy.value
		}
	}
}

fn test_zeroed() {
	a := [4]u8{}
	b := write_f32le(0.0)
	assert a == b
}

fn test_value() {
	mut a2 := [4]u8{}
	a2 = [u8(205), 204, 204, 61]!
	b2 := write_f32le(0.1)
	assert a2 == b2
}
