import toml

struct IntegerKeyConfig {
	nums map[int]string
}

struct KeyConfig[K] {
	nums map[K]string
}

fn check_keys[K](text string, expected map[K]string) {
	config := toml.decode[KeyConfig[K]](text)!
	assert config.nums == expected
}

fn test_decode_integer_map_keys() {
	config := toml.decode[IntegerKeyConfig]('nums = { 1 = "a", 2 = "b" }')!
	assert config.nums == {
		1: 'a'
		2: 'b'
	}
}

fn test_signed_integer_key_widths() {
	check_keys[i8]('nums = { "-128" = "min", 127 = "max", 128 = "skip", "-129" = "skip" }', {
		i8(-128): 'min'
		i8(127):  'max'
	})
	check_keys[i16]('nums = { "-32768" = "min", 32767 = "max", 32768 = "skip" }', {
		i16(-32768): 'min'
		i16(32767):  'max'
	})
	check_keys[int]('nums = { "-2147483648" = "min", 2147483647 = "max", 9223372036854775808 = "skip" }', {
		-2147483648: 'min'
		2147483647:  'max'
	})
	check_keys[i32]('nums = { "-2147483648" = "min", 2147483647 = "max", "-2147483649" = "skip" }', {
		i32(-2147483648): 'min'
		i32(2147483647):  'max'
	})
	check_keys[i64]('nums = { "-9223372036854775808" = "min", 9223372036854775807 = "max", 9223372036854775808 = "skip", "-9223372036854775809" = "skip" }', {
		i64(-9223372036854775807) - 1: 'min'
		i64(9223372036854775807):      'max'
	})
	check_keys[isize]('nums = { "-1" = "negative", 42 = "positive" }', {
		isize(-1): 'negative'
		isize(42): 'positive'
	})
}

fn test_unsigned_integer_key_widths() {
	check_keys[u8]('nums = { 0 = "zero", 255 = "max", 256 = "skip", "-1" = "skip" }', {
		u8(0):   'zero'
		u8(255): 'max'
	})
	check_keys[u16]('nums = { 65535 = "max", 65536 = "skip" }', {
		u16(65535): 'max'
	})
	check_keys[u32]('nums = { 4294967295 = "max", 4294967296 = "skip" }', {
		u32(4294967295): 'max'
	})
	check_keys[u64]('nums = { 18446744073709551615 = "max", 18446744073709551616 = "skip" }', {
		u64(18446744073709551615): 'max'
	})
	check_keys[usize]('nums = { 42 = "value", "-1" = "skip" }', {
		usize(42): 'value'
	})
}

fn test_invalid_integer_keys_do_not_overwrite_zero() {
	check_keys[int]('nums = { 0 = "zero", abc = "skip", "" = "skip", "+" = "skip", "-" = "skip", "1.5" = "skip", " 1" = "skip", "0x10" = "skip", "1_0" = "skip", "+7" = "seven" }', {
		0: 'zero'
		7: 'seven'
	})
}

struct MapItem {
	name string
}

struct CollectionKeyConfig {
	labels map[string]string
	flags  map[bool]int
	items  map[i64]MapItem
	counts map[int]u8
	nested map[int]map[u64]string
}

fn test_map_key_controls_and_nested_ownership() {
	config := toml.decode[CollectionKeyConfig]('labels = { one = "a" }
flags = { true = 9, false = 4, other = 2 }
items = { 4294967296 = { name = "wide" } }
counts = { 1 = 255, 2 = 256 }
nested = { 1 = { 18446744073709551615 = "wide" }, 2 = { 7 = "second" } }')!
	assert config.labels == {
		'one': 'a'
	}
	assert config.flags == {
		true:  9
		false: 4
	}
	assert config.items[i64(4294967296)].name == 'wide'
	assert config.counts == {
		1: u8(255)
	}
	assert config.nested[1] == {
		u64(18446744073709551615): 'wide'
	}
	assert config.nested[2] == {
		u64(7): 'second'
	}
}

struct OptionalMapValues {
	values map[int]?string = {
		1: ?string('default')
	}
}

fn test_unsupported_optional_map_values_keep_defaults() {
	for text in ['values = {}', 'values = { 2 = "ignored" }'] {
		value := toml.decode[OptionalMapValues](text)!
		assert value.values.len == 1
		assert (value.values[1] or { '' }) == 'default'
	}
}

type WideUnsignedKey = u64

fn test_unsigned_key_alias() {
	check_keys[WideUnsignedKey]('nums = { 18446744073709551615 = "wide" }', {
		WideUnsignedKey(18446744073709551615): 'wide'
	})
}
