// An untyped int constant next to a smaller integer takes that integer's type when its value
// fits, as in the checker and V 0.5.2: `u8(255) + c` is a u8 for `const c = 3`. Named
// constants, hexadecimal literals and parentheses used to make such an operation an `int`.

const small_three = 3
const small_sum = 1 + 2
const small_hex = 0x03
const small_paren = (3)
const small_alias = small_three
const small_negative = -3
const small_big = 300

fn test_u8_operands() {
	u := u8(255)
	assert u + small_three == 2
	assert small_three + u == 2
	assert u + small_sum == 2
	assert u + small_hex == 2
	assert u + small_paren == 2
	assert u + small_alias == 2
	assert u + 0x03 == 2
	assert u + (3) == 2
	assert typeof(u + small_three).name == 'u8'
	assert typeof(u + 0x03).name == 'u8'
	assert '${u + small_three}' == '2'
}

fn test_u16_operands() {
	u := u16(65535)
	assert u + small_three == 2
	assert typeof(u + small_three).name == 'u16'
}

fn test_signed_operands() {
	i := i8(100)
	assert i * 3 == 44
	assert i * small_three == 44
	assert i * -3 == -44
	assert i * small_negative == -44
	assert typeof(i * small_negative).name == 'i8'
	assert '${i * small_three}' == '44'
	h := i16(32767)
	assert h + small_three == -32766
	assert typeof(h + small_three).name == 'i16'
}

fn test_constants_out_of_range() {
	u := u8(255)
	assert u + small_big == 555
	assert typeof(u + small_big).name == 'int'
	assert u + -3 == 252
	assert typeof(u + -3).name == 'int'
	assert u - small_negative == 258
	assert typeof(u - small_negative).name == 'int'
	w := u32(1)
	assert w + small_negative == 4294967294
	assert typeof(w + small_negative).name == 'u32'
}
