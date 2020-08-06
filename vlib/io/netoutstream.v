module io

import net
import encoding.binary

struct NetOutputStream {
mut:
	sock &net.Socket
}

pub fn new_net_output_stream(sock &net.Socket) &NetOutputStream {
	return &NetOutputStream{
		sock: sock
	}
}

pub fn (mut nos NetOutputStream) write_int(d int) ? {
	mut bytes := []byte{len: int(sizeof(int))}
	binary.big_endian_put_u32(mut bytes, u32(d))
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_ints(d []int) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(int))}
		binary.big_endian_put_u32(mut tmp, u32(u))
		bytes << tmp
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_i8(d i8) ? {
	nos.sock.send([byte(d)].data, int(sizeof(i8)))?
}

pub fn (mut nos NetOutputStream) write_i8s(d []i8) ? {
	nos.sock.send(d.data, int(sizeof(i8) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_i16(d i16) ? {
	mut bytes := []byte{len: int(sizeof(i16))}
	binary.big_endian_put_u16(mut bytes, u16(d))
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_i16s(d []i16) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(i16))}
		binary.big_endian_put_u16(mut tmp, u16(u))
		bytes << tmp
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_i64(d i64) ? {
	mut bytes := []byte{len: int(sizeof(i64))}
	binary.big_endian_put_u64(mut bytes, u64(d))
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_i64s(d []i64) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(i64))}
		binary.big_endian_put_u64(mut tmp, u64(u))
		bytes << tmp
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_byte(d byte) ? {
	nos.sock.send([d].data, int(sizeof(byte)))?
}

pub fn (mut nos NetOutputStream) write_bytes(d []byte) ? {
	nos.sock.send(d.data, int(sizeof(byte) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_u16(d u16) ? {
	mut bytes := []byte{len: int(sizeof(u16))}
	binary.big_endian_put_u16(mut bytes, d)
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_u16s(d []u16) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(u16))}
		binary.big_endian_put_u16(mut tmp, u)
		bytes << tmp
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_u32(d u32) ? {
	mut bytes := []byte{len: int(sizeof(u32))}
	binary.big_endian_put_u32(mut bytes, d)
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_u32s(d []u32) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(u32))}
		binary.big_endian_put_u32(mut tmp, u)
		bytes << tmp
	}
	nos.sock.send(bytes.data, int(sizeof(u32) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_u64(d u64) ? {
	mut bytes := []byte{len: int(sizeof(u64))}
	binary.big_endian_put_u64(mut bytes, d)
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_u64s(d []u64) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(u64))}
		binary.big_endian_put_u64(mut tmp, u)
		bytes << tmp
	}
	nos.sock.send(bytes.data, int(sizeof(u64) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_f32(d f32) ? {
	pb := &byte(&d)
	mut bytes := []byte{len: int(sizeof(f32))}
	unsafe {
		for i in 0..bytes.len {
			bytes[i] = pb[i]
		}
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_f32s(d []f32) ? {
	mut bytes := []byte{}
	for f in d {
		pb := &byte(&f)
		unsafe {
			for i in 0..int(sizeof(f32)) {
				bytes << pb[i]
			}
		}
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_f64(d f64) ? {
	pb := &byte(&d)
	mut bytes := []byte{len: int(sizeof(f64))}
	unsafe {
		for i in 0..bytes.len {
			bytes[i] = pb[i]
		}
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_f64s(d []f64) ? {
	mut bytes := []byte{}
	for f in d {
		pb := &byte(&f)
		unsafe {
			for i in 0..int(sizeof(f64)) {
				bytes << pb[i]
			}
		}
	}
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_string(d string) ? {
	nos.write_bytes(d.bytes())?
}

// TODO make it generic
pub fn (mut nos NetOutputStream) write_struct(d voidptr, l u32) ? {
	nos.sock.send(byteptr(d), int(l))?
}
