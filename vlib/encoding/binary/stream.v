module binary

import io

// ByteOrder decodes and encodes fixed-size integers using a specific byte order.
pub interface ByteOrder {
	u16(b []u8) u16
	u32(b []u8) u32
	u64(b []u8) u64
	put_u16(mut b []u8, value u16)
	put_u32(mut b []u8, value u32)
	put_u64(mut b []u8, value u64)
}

// LittleEndian implements ByteOrder using little-endian encoding.
pub struct LittleEndian {}

// BigEndian implements ByteOrder using big-endian encoding.
pub struct BigEndian {}

pub const little_endian = LittleEndian{}
pub const big_endian = BigEndian{}

// u16 decodes a 16-bit unsigned integer from b using little-endian byte order.
pub fn (_ LittleEndian) u16(b []u8) u16 {
	return little_endian_u16(b)
}

// u32 decodes a 32-bit unsigned integer from b using little-endian byte order.
pub fn (_ LittleEndian) u32(b []u8) u32 {
	return little_endian_u32(b)
}

// u64 decodes a 64-bit unsigned integer from b using little-endian byte order.
pub fn (_ LittleEndian) u64(b []u8) u64 {
	return little_endian_u64(b)
}

// put_u16 encodes value into b using little-endian byte order.
pub fn (_ LittleEndian) put_u16(mut b []u8, value u16) {
	little_endian_put_u16(mut b, value)
}

// put_u32 encodes value into b using little-endian byte order.
pub fn (_ LittleEndian) put_u32(mut b []u8, value u32) {
	little_endian_put_u32(mut b, value)
}

// put_u64 encodes value into b using little-endian byte order.
pub fn (_ LittleEndian) put_u64(mut b []u8, value u64) {
	little_endian_put_u64(mut b, value)
}

// u16 decodes a 16-bit unsigned integer from b using big-endian byte order.
pub fn (_ BigEndian) u16(b []u8) u16 {
	return big_endian_u16(b)
}

// u32 decodes a 32-bit unsigned integer from b using big-endian byte order.
pub fn (_ BigEndian) u32(b []u8) u32 {
	return big_endian_u32(b)
}

// u64 decodes a 64-bit unsigned integer from b using big-endian byte order.
pub fn (_ BigEndian) u64(b []u8) u64 {
	return big_endian_u64(b)
}

// put_u16 encodes value into b using big-endian byte order.
pub fn (_ BigEndian) put_u16(mut b []u8, value u16) {
	big_endian_put_u16(mut b, value)
}

// put_u32 encodes value into b using big-endian byte order.
pub fn (_ BigEndian) put_u32(mut b []u8, value u32) {
	big_endian_put_u32(mut b, value)
}

// put_u64 encodes value into b using big-endian byte order.
pub fn (_ BigEndian) put_u64(mut b []u8, value u64) {
	big_endian_put_u64(mut b, value)
}

// read decodes fixed-size values from reader using order.
pub fn read[T](mut reader io.Reader, order ByteOrder, mut data T) ! {
	read_value(mut reader, order, mut data)!
}

// write encodes fixed-size values to writer using order.
pub fn write[T](mut writer io.Writer, order ByteOrder, data T) ! {
	write_value(mut writer, order, data)!
}

// size returns the number of bytes needed to encode data, or `-1` for unsupported types.
pub fn size[T](data T) int {
	return size_value(data)
}

fn size_value[T](data T) int {
	$if T is bool || T is u8 || T is i8 {
		return 1
	} $else $if T is u16 || T is i16 {
		return 2
	} $else $if T is u32 || T is i32 || T is f32 {
		return 4
	} $else $if T is u64 || T is i64 || T is f64 {
		return 8
	} $else $if T is $array_fixed {
		mut total := 0
		for i in 0 .. data.len {
			value_size := size_value(data[i])
			if value_size < 0 {
				return -1
			}
			total += value_size
		}
		return total
	} $else $if T is $array {
		mut total := 0
		for value in data {
			value_size := size(value)
			if value_size < 0 {
				return -1
			}
			total += value_size
		}
		return total
	} $else $if T is $struct {
		mut total := 0
		$for field in T.fields {
			if field.name == '_' {
				return -1
			}
			field_size := size(data.$(field.name))
			if field_size < 0 {
				return -1
			}
			total += field_size
		}
		return total
	} $else {
		return -1
	}
}

fn read_value[T](mut reader io.Reader, order ByteOrder, mut data T) ! {
	$if T is bool {
		mut buf := []u8{len: 1}
		read_full(mut reader, mut buf)!
		data = buf[0] != 0
	} $else $if T is u8 {
		mut buf := []u8{len: 1}
		read_full(mut reader, mut buf)!
		data = buf[0]
	} $else $if T is i8 {
		mut buf := []u8{len: 1}
		read_full(mut reader, mut buf)!
		data = i8(buf[0])
	} $else $if T is u16 {
		mut buf := []u8{len: 2}
		read_full(mut reader, mut buf)!
		data = order.u16(buf)
	} $else $if T is i16 {
		mut buf := []u8{len: 2}
		read_full(mut reader, mut buf)!
		data = i16(order.u16(buf))
	} $else $if T is u32 {
		mut buf := []u8{len: 4}
		read_full(mut reader, mut buf)!
		data = order.u32(buf)
	} $else $if T is i32 {
		mut buf := []u8{len: 4}
		read_full(mut reader, mut buf)!
		data = i32(order.u32(buf))
	} $else $if T is u64 {
		mut buf := []u8{len: 8}
		read_full(mut reader, mut buf)!
		data = order.u64(buf)
	} $else $if T is i64 {
		mut buf := []u8{len: 8}
		read_full(mut reader, mut buf)!
		data = i64(order.u64(buf))
	} $else $if T is f32 {
		mut buf := []u8{len: 4}
		read_full(mut reader, mut buf)!
		data = unsafe {
			U32_F32{
				u: order.u32(buf)
			}.f
		}
	} $else $if T is f64 {
		mut buf := []u8{len: 8}
		read_full(mut reader, mut buf)!
		data = unsafe {
			U64_F64{
				u: order.u64(buf)
			}.f
		}
	} $else $if T is $array_fixed {
		for i in 0 .. data.len {
			read_value(mut reader, order, mut data[i])!
		}
	} $else $if T is $array {
		$if T is []u8 {
			read_full(mut reader, mut data)!
		} $else {
			for i in 0 .. data.len {
				read_value(mut reader, order, mut data[i])!
			}
		}
	} $else $if T is $struct {
		$for field in T.fields {
			if field.name == '_' {
				return error('binary.read: structs with `_` fields are not supported')
			}
			read_value(mut reader, order, mut data.$(field.name))!
		}
	} $else {
		return error('binary.read: unsupported type ${typeof(data).name}')
	}
}

fn write_value[T](mut writer io.Writer, order ByteOrder, data T) ! {
	$if T is bool {
		write_full(mut writer, [u8(data)])!
	} $else $if T is u8 {
		write_full(mut writer, [u8(data)])!
	} $else $if T is i8 {
		write_full(mut writer, [u8(data)])!
	} $else $if T is u16 {
		mut buf := []u8{len: 2}
		order.put_u16(mut buf, data)
		write_full(mut writer, buf)!
	} $else $if T is i16 {
		mut buf := []u8{len: 2}
		order.put_u16(mut buf, u16(data))
		write_full(mut writer, buf)!
	} $else $if T is u32 {
		mut buf := []u8{len: 4}
		order.put_u32(mut buf, data)
		write_full(mut writer, buf)!
	} $else $if T is i32 {
		mut buf := []u8{len: 4}
		order.put_u32(mut buf, u32(data))
		write_full(mut writer, buf)!
	} $else $if T is u64 {
		mut buf := []u8{len: 8}
		order.put_u64(mut buf, data)
		write_full(mut writer, buf)!
	} $else $if T is i64 {
		mut buf := []u8{len: 8}
		order.put_u64(mut buf, u64(data))
		write_full(mut writer, buf)!
	} $else $if T is f32 {
		mut buf := []u8{len: 4}
		bits := unsafe {
			U32_F32{
				f: data
			}.u
		}
		order.put_u32(mut buf, bits)
		write_full(mut writer, buf)!
	} $else $if T is f64 {
		mut buf := []u8{len: 8}
		bits := unsafe {
			U64_F64{
				f: data
			}.u
		}
		order.put_u64(mut buf, bits)
		write_full(mut writer, buf)!
	} $else $if T is $array_fixed {
		for i in 0 .. data.len {
			write_value(mut writer, order, data[i])!
		}
	} $else $if T is $array {
		$if T is []u8 {
			write_full(mut writer, data)!
		} $else {
			for value in data {
				write_value(mut writer, order, value)!
			}
		}
	} $else $if T is $struct {
		$for field in T.fields {
			if field.name == '_' {
				return error('binary.write: structs with `_` fields are not supported')
			}
			write_value(mut writer, order, data.$(field.name))!
		}
	} $else {
		return error('binary.write: unsupported type ${typeof(data).name}')
	}
}

fn read_full(mut reader io.Reader, mut buf []u8) ! {
	mut offset := 0
	for offset < buf.len {
		n := reader.read(mut buf[offset..])!
		if n <= 0 || n > buf.len - offset {
			return error('binary.read: reader returned an invalid number of bytes')
		}
		offset += n
	}
}

fn write_full(mut writer io.Writer, buf []u8) ! {
	mut offset := 0
	for offset < buf.len {
		n := writer.write(buf[offset..])!
		if n <= 0 || n > buf.len - offset {
			return error('binary.write: writer returned an invalid number of bytes')
		}
		offset += n
	}
}
