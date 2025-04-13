module binary

fn test_encode_decode_primitive_string() ! {
	a_u8 := u8(137)
	a_u16 := u16(5325)
	a_u32 := u32(255421)
	a_u64 := u64(2483294832)
	a_i8 := i8(-11)
	a_i16 := i16(-2321)
	a_i32 := i32(-54322)
	a_i64 := i64(-54212245)
	a_int := int(-32135)
	a_f32 := f32(1.37)
	a_f64 := f64(-32144.3133)
	a_bool := bool(true)
	a_rune := `♥`
	a_isize := isize(-45433)
	a_usize := usize(432211)
	a_string := '♥🖊dsser333100'

	b_u8 := encode_binary(a_u8)!
	b_u16 := encode_binary(a_u16)!
	b_u32 := encode_binary(a_u32)!
	b_u64 := encode_binary(a_u64)!
	b_i8 := encode_binary(a_i8)!
	b_i16 := encode_binary(a_i16)!
	b_i32 := encode_binary(a_i32)!
	b_i64 := encode_binary(a_i64)!
	b_int := encode_binary(a_int)!
	b_f32 := encode_binary(a_f32)!
	b_f64 := encode_binary(a_f64)!
	b_bool := encode_binary(a_bool)!
	b_rune := encode_binary(a_rune)!
	b_isize := encode_binary(a_isize)!
	b_usize := encode_binary(a_usize)!
	b_string := encode_binary(a_string)!

	c_u8 := decode_binary[u8](b_u8)!
	c_u16 := decode_binary[u16](b_u16)!
	c_u32 := decode_binary[u32](b_u32)!
	c_u64 := decode_binary[u64](b_u64)!
	c_i8 := decode_binary[i8](b_i8)!
	c_i16 := decode_binary[i16](b_i16)!
	c_i32 := decode_binary[i32](b_i32)!
	c_i64 := decode_binary[i64](b_i64)!
	c_int := decode_binary[int](b_int)!
	c_f32 := decode_binary[f32](b_f32)!
	c_f64 := decode_binary[f64](b_f64)!
	c_bool := decode_binary[bool](b_bool)!
	c_rune := decode_binary[rune](b_rune)!
	c_isize := decode_binary[isize](b_isize)!
	c_usize := decode_binary[usize](b_usize)!
	c_string := decode_binary[string](b_string)!

	assert a_u8 == c_u8
	assert a_u16 == c_u16
	assert a_u32 == c_u32
	assert a_u64 == c_u64
	assert a_i8 == c_i8
	assert a_i16 == c_i16
	assert a_i32 == c_i32
	assert a_i64 == c_i64
	assert a_int == c_int
	assert a_f32 == c_f32
	assert a_f64 == c_f64
	assert a_bool == c_bool
	assert a_rune == c_rune
	assert a_isize == c_isize
	assert a_usize == c_usize
	assert a_string == c_string

	assert b_u8 == [u8(137)]
	assert b_u16 == [u8(205), 20]
	assert b_u32 == [u8(189), 229, 3, 0]
	assert b_u64 == [u8(112), 18, 4, 148, 0, 0, 0, 0]
	assert b_i8 == [u8(245)]
	assert b_i16 == [u8(239), 246]
	assert b_i32 == [u8(206), 43, 255, 255]
	assert b_i64 == [u8(107), 201, 196, 252, 255, 255, 255, 255]
	assert b_int == [u8(121), 130, 255, 255, 255, 255, 255, 255]
	assert b_f32 == [u8(41), 92, 175, 63]
	assert b_f64 == [u8(118), 113, 27, 13, 20, 100, 223, 192]
	assert b_bool == [u8(1)]
	assert b_rune == [u8(101), 38, 0, 0]
	assert b_string == [u8(18), 0, 0, 0, 0, 0, 0, 0, 226, 153, 165, 240, 159, 150, 138, 100, 115,
		115, 101, 114, 51, 51, 51, 49, 48, 48]
	$if x64 {
		assert b_isize == [u8(135), 78, 255, 255, 255, 255, 255, 255]
		assert b_usize == [u8(83), 152, 6, 0, 0, 0, 0, 0]
	} $else {
		assert b_isize == [u8(135), 78, 255, 255]
		assert b_usize == [u8(83), 152, 6, 0]
	}
}

fn test_encode_decode_array() {
	a_u8 := [u8(137), 21]
	a_u16 := [u16(5325), 322]
	a_u32 := [u32(255421), 34255]
	a_u64 := [u64(2483294832), 321554321]
	a_i8 := [i8(-11), 17]
	a_i16 := [i16(-2321), 6543]
	a_i32 := [i32(-54322), 23326]
	a_i64 := [i64(-54212245), 54223333]
	a_int := [int(-32135), 732561]
	a_f32 := [f32(1.37), -5442.3]
	a_f64 := [f64(-32144.3133), 432e-13]
	a_bool := [bool(true), false]
	a_rune := [`♥`, `🖊`]
	a_isize := [isize(-45433), 24342]
	a_usize := [usize(432211), 888533]
	a_string := ['♥', '🖊', 'dfd21']

	b_u8 := encode_binary(a_u8)!
	b_u16 := encode_binary(a_u16)!
	b_u32 := encode_binary(a_u32)!
	b_u64 := encode_binary(a_u64)!
	b_i8 := encode_binary(a_i8)!
	b_i16 := encode_binary(a_i16)!
	b_i32 := encode_binary(a_i32)!
	b_i64 := encode_binary(a_i64)!
	b_int := encode_binary(a_int)!
	b_f32 := encode_binary(a_f32)!
	b_f64 := encode_binary(a_f64)!
	b_bool := encode_binary(a_bool)!
	b_rune := encode_binary(a_rune)!
	b_isize := encode_binary(a_isize)!
	b_usize := encode_binary(a_usize)!
	b_string := encode_binary(a_string)!

	c_u8 := decode_binary[[]u8](b_u8)!
	c_u16 := decode_binary[[]u16](b_u16)!
	c_u32 := decode_binary[[]u32](b_u32)!
	c_u64 := decode_binary[[]u64](b_u64)!
	c_i8 := decode_binary[[]i8](b_i8)!
	c_i16 := decode_binary[[]i16](b_i16)!
	c_i32 := decode_binary[[]i32](b_i32)!
	c_i64 := decode_binary[[]i64](b_i64)!
	c_int := decode_binary[[]int](b_int)!
	c_f32 := decode_binary[[]f32](b_f32)!
	c_f64 := decode_binary[[]f64](b_f64)!
	c_bool := decode_binary[[]bool](b_bool)!
	c_rune := decode_binary[[]rune](b_rune)!
	c_isize := decode_binary[[]isize](b_isize)!
	c_usize := decode_binary[[]usize](b_usize)!
	c_string := decode_binary[[]string](b_string)!

	assert a_u8 == c_u8
	assert a_u16 == c_u16
	assert a_u32 == c_u32
	assert a_u64 == c_u64
	assert a_i8 == c_i8
	assert a_i16 == c_i16
	assert a_i32 == c_i32
	assert a_i64 == c_i64
	assert a_int == c_int
	assert a_f32 == c_f32
	assert a_f64 == c_f64
	assert a_bool == c_bool
	assert a_rune == c_rune
	assert a_isize == c_isize
	assert a_usize == c_usize
	assert a_string == c_string

	assert b_u8 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 137, 21]
	assert b_u16 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 205, 20, 66, 1]
	assert b_u32 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 189, 229, 3, 0, 207, 133, 0, 0]
	assert b_u64 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 112, 18, 4, 148, 0, 0, 0, 0, 145, 135, 42, 19,
		0, 0, 0, 0]
	assert b_i8 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 245, 17]
	assert b_i16 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 239, 246, 143, 25]
	assert b_i32 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 206, 43, 255, 255, 30, 91, 0, 0]
	assert b_i64 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 107, 201, 196, 252, 255, 255, 255, 255, 229, 97,
		59, 3, 0, 0, 0, 0]
	assert b_int == [u8(2), 0, 0, 0, 0, 0, 0, 0, 121, 130, 255, 255, 255, 255, 255, 255, 145, 45,
		11, 0, 0, 0, 0, 0]
	assert b_f32 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 41, 92, 175, 63, 102, 18, 170, 197]
	assert b_f64 == [u8(2), 0, 0, 0, 0, 0, 0, 0, 118, 113, 27, 13, 20, 100, 223, 192, 253, 251,
		253, 7, 220, 191, 199, 61]
	assert b_bool == [u8(2), 0, 0, 0, 0, 0, 0, 0, 1, 0]
	assert b_rune == [u8(2), 0, 0, 0, 0, 0, 0, 0, 101, 38, 0, 0, 138, 245, 1, 0]
	assert b_string == [u8(3), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 226, 153, 165, 4, 0,
		0, 0, 0, 0, 0, 0, 240, 159, 150, 138, 5, 0, 0, 0, 0, 0, 0, 0, 100, 102, 100, 50, 49]
	$if x64 {
		assert b_isize == [u8(2), 0, 0, 0, 0, 0, 0, 0, 135, 78, 255, 255, 255, 255, 255, 255, 22,
			95, 0, 0, 0, 0, 0, 0]
		assert b_usize == [u8(2), 0, 0, 0, 0, 0, 0, 0, 83, 152, 6, 0, 0, 0, 0, 0, 213, 142, 13,
			0, 0, 0, 0, 0]
	} $else {
		assert b_isize == [u8(2), 0, 0, 0, 0, 0, 0, 0, 135, 78, 255, 255, 22, 95, 0, 0]
		assert b_usize == [u8(2), 0, 0, 0, 0, 0, 0, 0, 83, 152, 6, 0, 213, 142, 13, 0]
	}
}

struct St {
	i int
}

fn test_encode_decode_map() {
	a_map_string_string := {
		'abc': 'def'
	}
	a_map_string_int := {
		'abc': int(21343)
	}
	a_map_string_u8 := {
		'abc': u8(37)
	}
	a_map_string_u16 := {
		'abc': u16(3347)
	}
	a_map_string_u32 := {
		'abc': u32(333347)
	}
	a_map_string_u64 := {
		'abc': u64(64423)
	}
	a_map_string_i8 := {
		'abc': i8(-37)
	}
	a_map_string_i16 := {
		'abc': i16(-3347)
	}
	a_map_string_i32 := {
		'abc': i32(-333347)
	}
	a_map_string_i64 := {
		'abc': i64(-64423)
	}
	a_map_string_f32 := {
		'abc': f32(1.543)
	}
	a_map_string_f64 := {
		'abc': f64(1.54e31)
	}
	a_map_string_bool := {
		'abc': true
	}
	a_map_string_rune := {
		'abc': `♥`
	}
	a_map_string_isize := {
		'abc': isize(-45433)
	}
	a_map_string_usize := {
		'abc': usize(432211)
	}

	a_map_string_struct := {
		's': St{1}
	}

	b_map_string_string := encode_binary(a_map_string_string)!
	b_map_string_int := encode_binary(a_map_string_int)!
	b_map_string_u8 := encode_binary(a_map_string_u8)!
	b_map_string_u16 := encode_binary(a_map_string_u16)!
	b_map_string_u32 := encode_binary(a_map_string_u32)!
	b_map_string_u64 := encode_binary(a_map_string_u64)!
	b_map_string_i8 := encode_binary(a_map_string_i8)!
	b_map_string_i16 := encode_binary(a_map_string_i16)!
	b_map_string_i32 := encode_binary(a_map_string_i32)!
	b_map_string_i64 := encode_binary(a_map_string_i64)!
	b_map_string_f32 := encode_binary(a_map_string_f32)!
	b_map_string_f64 := encode_binary(a_map_string_f64)!
	b_map_string_bool := encode_binary(a_map_string_bool)!
	b_map_string_rune := encode_binary(a_map_string_rune)!
	b_map_string_isize := encode_binary(a_map_string_isize)!
	b_map_string_usize := encode_binary(a_map_string_usize)!
	b_map_string_struct := encode_binary(a_map_string_struct)!

	c_map_string_string := decode_binary[map[string]string](b_map_string_string)!
	c_map_string_int := decode_binary[map[string]int](b_map_string_int)!
	c_map_string_u8 := decode_binary[map[string]u8](b_map_string_u8)!
	c_map_string_u16 := decode_binary[map[string]u16](b_map_string_u16)!
	c_map_string_u32 := decode_binary[map[string]u32](b_map_string_u32)!
	c_map_string_u64 := decode_binary[map[string]u64](b_map_string_u64)!
	c_map_string_i8 := decode_binary[map[string]i8](b_map_string_i8)!
	c_map_string_i16 := decode_binary[map[string]i16](b_map_string_i16)!
	c_map_string_i32 := decode_binary[map[string]i32](b_map_string_i32)!
	c_map_string_i64 := decode_binary[map[string]i64](b_map_string_i64)!
	c_map_string_f32 := decode_binary[map[string]f32](b_map_string_f32)!
	c_map_string_f64 := decode_binary[map[string]f64](b_map_string_f64)!
	c_map_string_bool := decode_binary[map[string]bool](b_map_string_bool)!
	c_map_string_rune := decode_binary[map[string]rune](b_map_string_rune)!
	c_map_string_isize := decode_binary[map[string]isize](b_map_string_isize)!
	c_map_string_usize := decode_binary[map[string]usize](b_map_string_usize)!
	c_map_string_struct := decode_binary[map[string]St](b_map_string_struct)!

	assert a_map_string_string == c_map_string_string
	assert a_map_string_int == c_map_string_int
	assert a_map_string_u8 == c_map_string_u8
	assert a_map_string_u16 == c_map_string_u16
	assert a_map_string_u32 == c_map_string_u32
	assert a_map_string_i8 == c_map_string_i8
	assert a_map_string_i16 == c_map_string_i16
	assert a_map_string_i32 == c_map_string_i32
	assert a_map_string_i64 == c_map_string_i64
	assert a_map_string_f32 == c_map_string_f32
	assert a_map_string_f64 == c_map_string_f64
	assert a_map_string_bool == c_map_string_bool
	assert a_map_string_rune == c_map_string_rune
	assert a_map_string_isize == c_map_string_isize
	assert a_map_string_usize == c_map_string_usize
	assert a_map_string_struct == c_map_string_struct

	assert b_map_string_string == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98,
		99, 3, 0, 0, 0, 0, 0, 0, 0, 100, 101, 102]
	assert b_map_string_int == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		95, 83, 0, 0, 0, 0, 0, 0]
	assert b_map_string_u8 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		37]
	assert b_map_string_u16 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		19, 13]
	assert b_map_string_u32 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		35, 22, 5, 0]
	assert b_map_string_u64 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		167, 251, 0, 0, 0, 0, 0, 0]
	assert b_map_string_i8 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		219]
	assert b_map_string_i16 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		237, 242]
	assert b_map_string_i32 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		221, 233, 250, 255]
	assert b_map_string_i64 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		89, 4, 255, 255, 255, 255, 255, 255]
	assert b_map_string_f32 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		6, 129, 197, 63]
	assert b_map_string_f64 == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		212, 186, 221, 173, 2, 76, 104, 70]
	assert b_map_string_bool == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		1]
	assert b_map_string_rune == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98, 99,
		101, 38, 0, 0]
	$if x64 {
		assert b_map_string_isize == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98,
			99, 135, 78, 255, 255, 255, 255, 255, 255]
		assert b_map_string_usize == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98,
			99, 83, 152, 6, 0, 0, 0, 0, 0]
	} $else {
		assert b_map_string_isize == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98,
			99, 135, 78, 255, 255]
		assert b_map_string_usize == [u8(1), 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 98,
			99, 83, 152, 6, 0]
	}
	assert b_map_string_struct == [u8(1), 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 115, 1,
		0, 0, 0, 0, 0, 0, 0]
}

struct MyStruct {
	f_u8           u8
	f_u16          u16
	f_u32          u32
	f_u64          u64
	f_i8           i8
	f_i16          i16
	f_i32          i32
	f_i64          i64
	f_int          int
	f_f32          f32
	f_f64          f64
	f_bool         bool
	f_rune         rune
	f_isize        isize
	f_usize        usize
	f_string       string
	f_array_u8     []u8
	f_array_string []string
}

fn test_encode_decode_struct() {
	a_struct := MyStruct{
		f_u8:           u8(31)
		f_u16:          u16(57)
		f_u32:          u32(6432)
		f_u64:          u64(7896423)
		f_i8:           i8(-22)
		f_i16:          i16(-5433)
		f_i32:          i32(-54244)
		f_i64:          i64(-8322234)
		f_int:          int(4235)
		f_f32:          f32(1.5382)
		f_f64:          f64(22421.32)
		f_bool:         bool(true)
		f_rune:         rune(`♥`)
		f_isize:        isize(42323)
		f_usize:        usize(83842)
		f_string:       'fds♥323s'
		f_array_u8:     [u8(32), 22, 55, 72]
		f_array_string: ['dfdss', 'dfsd3', '54344']
	}

	b_struct := encode_binary(a_struct)!
	c_struct := decode_binary[MyStruct](b_struct)!

	assert a_struct == c_struct
}

struct MyStruct_SkipFields {
mut:
	f_u8           u8
	f_u16          u16 @[serialize: '-']
	f_u32          u32
	f_u64          u64
	f_i8           i8
	f_i16          i16
	f_i32          i32
	f_i64          i64
	f_int          int
	f_f32          f32
	f_f64          f64
	f_bool         bool
	f_rune         rune
	f_isize        isize
	f_usize        usize
	f_string       string
	f_array_u8     []u8 @[serialize: '-']
	f_array_string []string
}

fn test_encode_decode_struct_skip_fields() {
	a_struct := MyStruct_SkipFields{
		f_u8:           u8(31)
		f_u16:          u16(57)
		f_u32:          u32(6432)
		f_u64:          u64(7896423)
		f_i8:           i8(-22)
		f_i16:          i16(-5433)
		f_i32:          i32(-54244)
		f_i64:          i64(-8322234)
		f_int:          int(4235)
		f_f32:          f32(1.5382)
		f_f64:          f64(22421.32)
		f_bool:         bool(true)
		f_rune:         rune(`♥`)
		f_isize:        isize(42323)
		f_usize:        usize(83842)
		f_string:       'fds♥323s'
		f_array_u8:     [u8(32), 22, 55, 72]
		f_array_string: ['dfdss', 'dfsd3', '54344']
	}

	b_struct := encode_binary(a_struct)!
	mut c_struct := decode_binary[MyStruct_SkipFields](b_struct)!

	assert a_struct != c_struct

	c_struct.f_u16 = u16(57)
	c_struct.f_array_u8 = [u8(32), 22, 55, 72]
	assert a_struct == c_struct
}

struct ComplexStruct {
	f_u8      u8
	f_u32     u32
	f_u64     u64
	f_string  string
	f_structs []MyStruct
	f_maps    []map[string]string
}

fn test_encode_decode_complex() {
	a_complex := ComplexStruct{
		f_u8:      u8(78)
		f_u32:     u32(0x11223344)
		f_u64:     u64(0x55667788)
		f_string:  'complex'
		f_structs: [
			MyStruct{
				f_u8:           u8(31)
				f_u16:          u16(57)
				f_u32:          u32(6432)
				f_u64:          u64(7896423)
				f_i8:           i8(-22)
				f_i16:          i16(-5433)
				f_i32:          i32(-54244)
				f_i64:          i64(-8322234)
				f_int:          int(4235)
				f_f32:          f32(1.5382)
				f_f64:          f64(22421.32)
				f_bool:         bool(true)
				f_rune:         rune(`♥`)
				f_isize:        isize(42323)
				f_usize:        usize(83842)
				f_string:       'fds♥323s'
				f_array_u8:     [u8(32), 22, 55, 72]
				f_array_string: ['dfdss', 'dfsd3', '54344']
			},
			MyStruct{
				f_u8:           u8(41)
				f_u16:          u16(67)
				f_u32:          u32(7432)
				f_u64:          u64(8896423)
				f_i8:           i8(-32)
				f_i16:          i16(-6433)
				f_i32:          i32(-64244)
				f_i64:          i64(-9322234)
				f_int:          int(5235)
				f_f32:          f32(2.5382)
				f_f64:          f64(32421.32)
				f_bool:         bool(true)
				f_rune:         rune(`♥`)
				f_isize:        isize(52323)
				f_usize:        usize(93842)
				f_string:       'ads♥323s'
				f_array_u8:     [u8(22), 22, 55, 72]
				f_array_string: ['dxfdss', 'dsefsd3', 'po54344']
			},
		]
		f_maps:    [
			{
				'abc': 'def'
			},
			{
				'123': '456'
			},
			{
				',./': '!@#'
			},
		]
	}

	b_complex := encode_binary(a_complex)!
	c_complex := decode_binary[ComplexStruct](b_complex)!

	assert a_complex == c_complex

	// big endian test
	b_complex_big_endian := encode_binary(a_complex, big_endian: true)!
	c_complex_big_endian := decode_binary[ComplexStruct](b_complex_big_endian, big_endian: true)!

	assert b_complex != b_complex_big_endian
	assert a_complex == c_complex_big_endian
}
