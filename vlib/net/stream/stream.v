module stream

pub interface InputStream {
	get_int() int
	get_ints(l u32) []int
	get_i8() i8
	get_i8s(l u32) []i8
	get_i16() i16
	get_i16s(l u32) []i16
	get_i64() i64
	get_i64s(l u32) []i64
	get_byte() byte
	get_bytes(l u32) []byte
	get_u16() u16
	get_u16s(l u32) []u16
	get_u32() u32
	get_u32s(l u32) []u32
	get_u64() u64
	get_u64s(l u32) []u64
	get_f32() f32
	get_f32s(l u32) []f32
	get_f64() f64
	get_f64s(l u32) []f64
	get_string(l u32)
	skip(l u32)
}

pub interface OutputStream {
	write_int(d int) ?
	write_ints(d []int) ?
	write_i8(d i8) ?
	write_i8s(d []i8) ?
	write_i16(d i16) ?
	write_i16s(d []i16) ?
	write_i64(d i64) ?
	write_i64s(d []i64) ?
	write_byte(d byte) ?
	write_bytes(d []byte) ?
	write_u16(d u16) ?
	write_u16s(d []u16) ?
	write_u32(d u32) ?
	write_u32s(d []u32) ?
	write_u64(d u64) ?
	write_u64s(d []u64) ?
	write_f32(d f32) ?
	write_f32s(d []f32) ?
	write_f64(d f64) ?
	write_f64s(d []f64) ?
	write_string(d string) ?
}
