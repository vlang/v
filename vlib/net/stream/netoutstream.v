module stream

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
	nos.sock.send(byteptr(d), int(sizeof(int)))?
}

pub fn (mut nos NetOutputStream) write_ints(d []int) ? {
	nos.sock.send(d.data, int(sizeof(int) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_i8(d i8) ? {
	nos.sock.send(byteptr(d), int(sizeof(i8)))?
}

pub fn (mut nos NetOutputStream) write_i8s(d []i8) ? {
	nos.sock.send(d.data, int(sizeof(i8) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_i16(d i16) ? {
	nos.sock.send(byteptr(d), int(sizeof(i16)))?
}

pub fn (mut nos NetOutputStream) write_i16s(d []i16) ? {
	nos.sock.send(d.data, int(sizeof(i16) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_i64(d i64) ? {
	nos.sock.send(byteptr(d), int(sizeof(i64)))?
}

pub fn (mut nos NetOutputStream) write_i64s(d []i64) ? {
	nos.sock.send(d.data, int(sizeof(i64) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_byte(d byte) ? {
	nos.sock.send(byteptr(d), int(sizeof(byte)))?
}

pub fn (mut nos NetOutputStream) write_bytes(d []byte) ? {
	nos.sock.send(d.data, int(sizeof(byte) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_u16(d u16) ? {
	nos.sock.send(byteptr(d), int(sizeof(u16)))?
}

pub fn (mut nos NetOutputStream) write_u16s(d []u16) ? {
	mut bytes := []byte{}
	for u in d {
		mut tmp := []byte{len: int(sizeof(u16))}
		binary.big_endian_put_u16(mut tmp, u)
		bytes << tmp
	}
	for b in bytes {
		print(int(b))
		print(' ')
	}
	println('')
	println(bytes.len)
	println(bytes.hex())
	nos.sock.send(bytes.data, bytes.len)?
}

pub fn (mut nos NetOutputStream) write_u32(d u32) ? {
	nos.sock.send(byteptr(d), int(sizeof(u32)))?
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
	nos.sock.send(byteptr(d), int(sizeof(u64)))?
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
	nos.sock.send(byteptr(int(d)), int(sizeof(f32)))?
}

pub fn (mut nos NetOutputStream) write_f32s(d []f32) ? {
	nos.sock.send(d.data, int(sizeof(f32) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_f64(d f64) ? {
	nos.sock.send(byteptr(int(d)), int(sizeof(f64)))?
}

pub fn (mut nos NetOutputStream) write_f64s(d []f64) ? {
	nos.sock.send(d.data, int(sizeof(f64) * u32(d.len)))?
}

pub fn (mut nos NetOutputStream) write_string(d string) ? {
	nos.write_bytes(d.bytes())?
}

pub fn (mut nos NetOutputStream) write_struct<T>(d T) ? {
	nos.sock.send(byteptr(d), int(sizeof(T)))?
}
