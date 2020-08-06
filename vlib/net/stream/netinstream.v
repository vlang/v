module stream

import net
import encoding.binary

struct NetInputStream {
mut:
	sock &net.Socket
}

pub fn new_net_input_stream(sock &net.Socket) &NetInputStream {
	return &NetInputStream{
		sock: sock
	}
}

pub fn (mut nis NetInputStream) get_int() int {
	return int(nis.get_bytes(sizeof(int)).data)
}

pub fn (mut nis NetInputStream) get_ints(l u32) []int {
	bytes := nis.get_bytes(sizeof(int) * l)
	mut ints := []int{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(int))
		b := bytes[offs..int(u32(offs) + sizeof(int))]
		ints << int(binary.little_endian_u32(b))
	}
	return ints
}

pub fn (mut nis NetInputStream) get_i8() i8 {
	return i8(nis.get_byte())
}

pub fn (mut nis NetInputStream) get_i8s(l u32) []i8 {
	bytes := nis.get_bytes(sizeof(i8) * l)
	mut i8s := []i8{}
	for i in 0 .. l {
		i8s << i8(bytes[i])
	}
	return i8s
}

pub fn (mut nis NetInputStream) get_i16() i16 {
	return i16(nis.get_bytes(sizeof(i16)).data)
}

pub fn (mut nis NetInputStream) get_i16s(l u32) []i16 {
	bytes := nis.get_bytes(sizeof(i16) * l)
	mut i16s := []i16{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(i16))
		b := bytes[offs..int(u32(offs) + sizeof(i16))]
		i16s << i16(binary.little_endian_u16(b))
	}
	return i16s
}

pub fn (mut nis NetInputStream) get_i64() i64 {
	return i64(nis.get_bytes(sizeof(i64)).data)
}

pub fn (mut nis NetInputStream) get_i64s(l u32) []i64 {
	bytes := nis.get_bytes(sizeof(i64) * l)
	mut i64s := []i64{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(i64))
		b := bytes[offs..int(u32(offs) + sizeof(i64))]
		i64s << i64(binary.little_endian_u64(b))
	}
	return i64s
}

pub fn (mut nis NetInputStream) get_byte() byte {
	ptr, _ := nis.sock.recv(int(sizeof(byte)))
	unsafe {
		return ptr[0]
	}
}

pub fn (mut nis NetInputStream) get_bytes(l u32) []byte {
	mut buf := byteptr(0)
	unsafe {
		buf = malloc(int(l))
	}
	nis.sock.cread(buf, int(l))
	println(string(buf).bytes().hex())
	return string(buf).bytes()
}

pub fn (mut nis NetInputStream) get_u16() u16 {
	return u16(nis.get_bytes(sizeof(u16)).data)
}

pub fn (mut nis NetInputStream) get_u16s(l u32) []u16 {
	bytes := nis.get_bytes(sizeof(u16) * l)
	println(bytes.len)
	mut u16s := []u16{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(u16))
		b := bytes[offs..int(u32(offs) + sizeof(u16))]
		u16s << binary.little_endian_u16(b)

	}
	return u16s
}

pub fn (mut nis NetInputStream) get_u32() u32 {
	return u32(nis.get_bytes(sizeof(u32)).data)
}

pub fn (mut nis NetInputStream) get_u32s(l u32) []u32 {
	bytes := nis.get_bytes(sizeof(u32) * l)
	println(bytes.len)
	mut u32s := []u32{}
	for i in 0 .. l {
		println(i)
		offs := int(u32(i) * sizeof(u32))
		b := bytes[offs..int(u32(offs) + sizeof(u32))]
		u32s <<  binary.little_endian_u32(b)
	}
	return u32s
}

pub fn (mut nis NetInputStream) get_u64() u64 {
	return u64(nis.get_bytes(sizeof(u64)).data)
}

pub fn (mut nis NetInputStream) get_u64s(l u32) []u64 {
	bytes := nis.get_bytes(sizeof(u64) * l)
	println(bytes.len)
	mut u64s := []u64{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(u64))
		b := bytes[offs..int(u32(offs) + sizeof(u64))]
		u64s <<  binary.little_endian_u64(b)
	}
	return u64s
}

pub fn (mut nis NetInputStream) get_f32() f32 {
	return f32(nis.get_int())
}

pub fn (mut nis NetInputStream) get_f32s(l u32) []f32 {
	bytes := nis.get_bytes(sizeof(f32) * l)
	mut f32s := []f32{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(f32))
		b := bytes[offs..int(u32(offs) + sizeof(f32))]
		f := &f32(b.data)
		unsafe {
			f32s << f[0]
		}
	}
	return f32s
}

pub fn (mut nis NetInputStream) get_f64() f64 {
	return f64(nis.get_i64())
}

pub fn (mut nis NetInputStream) get_f64s(l u32) []f64 {
	bytes := nis.get_bytes(sizeof(f64) * l)
	mut f64s := []f64{}
	for i in 0 .. l {
		offs := int(u32(i) * sizeof(f64))
		b := bytes[offs..int(u32(offs) + sizeof(f64))]
		f := &f64(b.data)
		unsafe {
			f64s << f[0]
		}
	}
	return f64s
}

pub fn (mut nis NetInputStream) get_string(l u32) string {
	return string(nis.get_bytes(l))
}

pub fn (mut nis NetInputStream) skip(l u32) {
	nis.get_bytes(l)
}

pub fn (mut nis NetInputStream) get_struct<T>() T {
	return T(nis.get_bytes(sizeof(T)).data)
}
