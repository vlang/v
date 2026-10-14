@[has_globals]
module main

enum SlotColor {
	red
	green
	blue
}

enum SlotByte as u8 {
	low
	high = 200
}

enum SlotShort as i16 {
	low = -300
	high
}

enum SlotWide as i64 {
	low = -5000000000
	high
}

@[flag]
enum SlotFlags {
	read
	write
	exec
}

type SlotCode = i32

type SlotScalar = SlotColor | i32 | u32

// The neighbours of a narrow argument in the tests: all ones next to a non negative value
// and zero next to a negative one, so that bytes read past a slot shorter than an `int`
// do not look like the sign extension of its value.
const ones = i64(-1)
const zero = u64(0)

// The tail of a `...voidptr` parameter has no element types, so a callee reads every
// integer argument as an `int`, like `strconv.v_sprintf` does.
fn read_ints(args ...voidptr) []int {
	mut values := []int{cap: args.len}
	for arg in args {
		values << unsafe { *(&int(arg)) }
	}
	return values
}

fn read_i64s(args ...voidptr) []i64 {
	mut values := []i64{cap: args.len}
	for arg in args {
		values << unsafe { *(&i64(arg)) }
	}
	return values
}

fn read_generic[T](value T) int {
	return read_ints(ones, value, ones)[1]
}

// A global initializer is lowered by the C generator instead of the transformer.
__global global_init_slots = read_ints(i64(-1), i32(-1), u32(4000000000), `€`, u8(200), i16(-3),
	SlotColor.blue, SlotByte.high, 42, i64(-1))

fn test_signed_arguments_narrower_than_int_are_sign_extended() {
	a := i8(-128)
	b := i16(-32768)
	c := i32(-2147483648)
	assert read_ints(a, b, c) == [int(a), int(b), int(c)]
	assert read_ints(zero, i8(-1), zero, i16(-1), zero) == [0, -1, 0, -1, 0]
	assert read_ints(zero, i32(-1), zero) == [0, -1, 0]
	assert read_ints(ones, i8(127), ones, i16(32767), ones) == [-1, 127, -1, 32767, -1]
	assert read_ints(ones, i32(2147483647), ones) == [-1, 2147483647, -1]
}

fn test_unsigned_arguments_narrower_than_int_are_zero_extended() {
	a := u8(255)
	b := u16(65535)
	c := u32(4294967295)
	assert read_ints(a, b, c) == [int(a), int(b), int(c)]
	assert read_ints(ones, u8(1), ones, u16(2), ones, u32(3), ones) == [-1, 1, -1, 2, -1, 3, -1]
	assert read_ints(ones, u32(4000000000), ones) == [-1, int(u32(4000000000)), -1]
}

fn test_rune_and_char_arguments_fill_an_int() {
	r := `€`
	assert read_ints(ones, r, ones, `a`, ones) == [-1, 0x20ac, -1, 97, -1]
	assert read_ints(ones, rune(0x10ffff), ones, char(66), ones) == [-1, 0x10ffff, -1, 66, -1]
}

fn test_enum_and_alias_arguments_fill_an_int() {
	color := SlotColor.green
	assert read_ints(ones, color, ones, SlotColor.blue, ones) == [-1, 1, -1, 2, -1]
	assert read_ints(ones, SlotByte.high, ones, SlotFlags.exec, ones) == [-1, 200, -1, 4, -1]
	assert read_ints(zero, SlotShort.low, zero, SlotCode(-9), zero) == [0, -300, 0, -9, 0]
}

fn test_generic_and_smartcast_arguments_fill_an_int() {
	assert read_generic(i32(7)) == 7
	assert read_generic(u32(4000000000)) == int(u32(4000000000))
	assert read_generic(`€`) == 0x20ac
	assert read_generic(SlotColor.blue) == 2
	assert read_generic(SlotByte.high) == 200
	color := SlotScalar(SlotColor.blue)
	if color is SlotColor {
		assert read_ints(ones, color, ones) == [-1, 2, -1]
	}
	number := SlotScalar(i32(-7))
	if number is i32 {
		assert read_ints(zero, number, zero) == [0, -7, 0]
	}
}

fn test_arguments_as_wide_as_an_int_keep_their_storage() {
	wide := i64(-5000000000)
	assert read_i64s(wide, u64(5000000000), SlotWide.low) == [wide, 5000000000, wide]
}

fn test_global_initializer_arguments_fill_an_int() {
	assert global_init_slots == [-1, -1, int(u32(4000000000)), 0x20ac, 200, -3, 2, 200, 42, -1]
}
