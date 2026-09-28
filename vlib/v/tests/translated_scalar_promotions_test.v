@[translated]
module main

type TranslatedAddress = u64

struct SentinelFields {
	default_address TranslatedAddress = -1
	address         TranslatedAddress
}

fn translated_address(value TranslatedAddress) TranslatedAddress {
	return value
}

fn test_translated_unsigned_sentinels() {
	mut address := TranslatedAddress(0)
	address = -1
	assert address == u64(0xffffffffffffffff)
	assert address == -1
	assert -1 == address
	negative := i64(-1)
	assert address == negative
	assert negative == address
	assert !(negative < address)
	assert translated_address(-1) == address
	fields := SentinelFields{ address: -1 }
	assert fields.address == address
	assert fields.default_address == address
}

fn test_translated_boolean_indices_and_pointer_offsets() {
	values := [11, 22, 33]!
	index := true
	assert values[index] == 22
	text := c'ab'
	assert text[index] == `b`
	mut ptr := unsafe { &values[0] }
	ptr += true
	assert *ptr == 22
	ptr -= true
	assert *ptr == 11
	mut raw := voidptr(ptr)
	raw += 1
	assert usize(raw) == usize(ptr) + 1
	raw -= 1
	assert raw == voidptr(ptr)
}

enum TranslatedPointerOffset {
	one = 1
}

fn test_translated_integral_pointer_expressions() {
	values := [11, 22, 33]!
	start := unsafe { &values[0] }
	assert *(start + true) == 22
	assert *(start + char(2)) == 33
	assert *(start + TranslatedPointerOffset.one) == 22
	assert *(true + start) == 22
	end := unsafe { &values[2] }
	assert *(end - true) == 22
}

fn translated_mixed_branch(flag bool) int {
	value := if flag { flag } else { 42 }
	return value
}

fn test_translated_mixed_scalar_branches() {
	assert translated_mixed_branch(true) == 1
	assert translated_mixed_branch(false) == 42
	flag := false
	value := if flag { 42 } else { flag }
	assert value == 0
	floating := if flag { true } else { 2.5 }
	assert floating == 2.5
	chained := if flag {
		true
	} else if !flag {
		42
	} else {
		false
	}
	assert chained == 42
}

fn test_translated_narrow_integer_shift() {
	value := u16(2)
	assert int(value << 16) == 131072
	shifted := value << 16
	assert shifted == 131072
}

type TranslatedHandler = fn (int) int

fn translated_increment(value int) int {
	return value + 1
}

fn translated_handler_address() voidptr {
	return voidptr(translated_increment)
}

fn test_translated_callback_address_assignment() {
	mut callback := TranslatedHandler(unsafe { nil })
	assert callback == 0
	assert 0 == callback
	assert callback != -1
	assert -1 != callback
	callback = translated_handler_address()
	assert callback(41) == 42
	assert callback != 0
	assert callback != -1
}

enum TranslatedScalarKind {
	zero
	value = 42
}

fn test_translated_mixed_fixed_width_and_enum_branches() {
	flag := false
	fixed := if flag { true } else { i32(42) }
	assert fixed == 42
	kind := if flag { true } else { TranslatedScalarKind.value }
	assert int(kind) == 42
	character := if flag { true } else { char(42) }
	assert int(character) == 42
	reversed := if !flag { TranslatedScalarKind.value } else { true }
	assert int(reversed) == 42
}

fn translated_float_return(value f64) int {
	return value
}

fn translated_negative_unsigned_return() u64 {
	return -1
}

fn test_translated_scalar_return_conversions() {
	assert translated_float_return(4.75) == 4
	assert translated_negative_unsigned_return() == u64(0xffffffffffffffff)
}

fn test_translated_unary_and_shift_integral_promotions() {
	kind := TranslatedScalarKind.value
	character := char(2)
	negative_bool := -true
	assert negative_bool == -1
	assert typeof(negative_bool).name == 'int'
	assert int(~kind) == -43
	assert int(-kind) == -42
	assert int(~character) == -3
	assert int(-character) == -2
	bool_sum := true + 2
	assert bool_sum == 3
	assert typeof(bool_sum).name == 'int'
	assert int(kind << 1) == 84
	assert int(character >> 1) == 1
	assert int(1 << true) == 2
	bool_shifted := true << 1
	assert bool_shifted == 2
	assert typeof(bool_shifted).name == 'i32'
}

fn test_translated_conditional_uses_common_numeric_type() {
	flag := false
	value := if flag { 1 } else { 2.5 }
	assert value == 2.5
	assert typeof(value).name == 'f64'
}
