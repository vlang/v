import math
import toml

struct WideIntegerValues {
	unsigned        u64 = 11
	signed          i64 = -7
	machine         int = -9
	optional        ?u64
	optional_signed ?i64
	values          []u64
	signed_values   []i64
	by_name         map[string]u64
	signed_by_name  map[string]i64
}

fn test_wide_integer_values_are_preserved() {
	for literal in ['9223372036854775808', '+9223372036854775808', '18_446_744_073_709_551_615',
		'0xFFFF_FFFF_FFFF_FFFF', '0o1777777777777777777777',
		'0b1111111111111111111111111111111111111111111111111111111111111111'] {
		expected := if literal.contains('9223372036854775808') {
			u64(9223372036854775808)
		} else {
			u64(18446744073709551615)
		}
		doc := toml.parse_text('unsigned = ${literal}\noptional = ${literal}')!
		value := doc.value('unsigned')
		assert value is u64
		assert value.u64() == expected
		decoded := doc.decode[WideIntegerValues]()!
		assert decoded.unsigned == expected
		assert decoded.optional? == expected
	}
	for literal in ['-9223372036854775808', '9223372036854775807'] {
		doc := toml.parse_text('signed = ${literal}')!
		assert doc.value('signed') is i64
		assert doc.value('signed').i64().str() == literal
	}
}

fn test_integer_overflow_is_rejected_before_values_are_converted() {
	for literal in ['18446744073709551616', '+18446744073709551616', '-9223372036854775809',
		'-18446744073709551616', '0x10000000000000000', '0o2000000000000000000000',
		'0b10000000000000000000000000000000000000000000000000000000000000000'] {
		for text in ['value = ${literal}', 'values = [1, ${literal}]',
			'values = { nested = ${literal} }'] {
			if _ := toml.parse_text(text) {
				assert false, 'expected integer overflow to be rejected: ${text}'
			}
		}
	}
}

fn test_wide_integer_fields_and_collections_skip_values_outside_their_range() {
	decoded := toml.decode[WideIntegerValues]('unsigned = -5
signed = 9223372036854775808
machine = 18446744073709551615
optional = -1
optional_signed = 18446744073709551615
values = [0, -1, 18446744073709551615]
signed_values = [-9223372036854775808, 9223372036854775808, 7]
by_name = { valid = 18446744073709551615, negative = -5 }
signed_by_name = { valid = 7, overflow = 18446744073709551615 }
')!
	assert decoded.unsigned == 11
	assert decoded.signed == -7
	assert decoded.machine == -9
	assert decoded.optional == none
	assert decoded.optional_signed == none
	assert decoded.values == [u64(0), u64(18446744073709551615)]
	assert decoded.signed_values == [i64(-9223372036854775807) - 1, i64(7)]
	assert decoded.by_name == {
		'valid': u64(18446744073709551615)
	}
	assert decoded.signed_by_name == {
		'valid': i64(7)
	}
}

fn test_any_wide_integer_conversions_do_not_wrap() {
	doc := toml.parse_text('negative = -5\nlarge = 18446744073709551615')!
	assert doc.value('negative').u64() == 0
	assert doc.value('large').i64() == 0
	assert doc.value('large').int() == 0
	assert doc.value('large').f64() == f64(u64(18446744073709551615))
	assert doc.value('large').f32() == f32(u64(18446744073709551615))
	assert toml.Any(u64(7)).i64() == 7
	assert toml.Any(u64(7)).int() == 7
	if sizeof(int) == 4 {
		assert toml.Any(i64(2147483648)).int() == 0
	}
}

fn test_negative_zero_and_incompatible_wide_integer_values() {
	zero := toml.decode[WideIntegerValues]('unsigned = -0\nsigned = -0\noptional = -0')!
	assert zero.unsigned == 0
	assert zero.signed == 0
	assert zero.optional? == 0
	quoted_zero := toml.decode[WideIntegerValues]('unsigned = "-0"\noptional = "-0"')!
	assert quoted_zero.unsigned == 0
	assert quoted_zero.optional? == 0
	for literal in ['nan', 'inf', '-inf', '1.5', 'true', '"18446744073709551616"'] {
		decoded := toml.decode[WideIntegerValues]('unsigned = ${literal}
signed = ${literal}
optional = ${literal}
values = [7, ${literal}]
signed_values = [7, ${literal}]
by_name = { valid = 7, invalid = ${literal} }
signed_by_name = { valid = 7, invalid = ${literal} }
')!
		assert decoded.unsigned == 11, literal
		assert decoded.signed == -7, literal
		assert decoded.optional == none, literal
		assert decoded.values == [u64(7)], literal
		assert decoded.signed_values == [i64(7)], literal
		assert decoded.by_name == {
			'valid': u64(7)
		}, literal
		assert decoded.signed_by_name == {
			'valid': i64(7)
		}, literal
	}
}

struct SpecialFloatValues {
	value    f64
	narrow   f32
	optional ?f64
	values   []f64
	by_name  map[string]f64
}

fn test_special_floats_remain_floats_in_documents_and_typed_values() {
	for literal in ['nan', '+nan', '-nan', 'inf', '+inf', '-inf'] {
		doc := toml.parse_text('value = ${literal}
narrow = ${literal}
optional = ${literal}
values = [${literal}]
by_name = { value = ${literal} }
')!
		assert doc.value('value') is f64
		decoded := doc.decode[SpecialFloatValues]()!
		values := [doc.value('value').f64(), decoded.value, f64(decoded.narrow), decoded.optional?,
			decoded.values[0], decoded.by_name['value']]
		for value in values {
			if literal.ends_with('nan') {
				assert math.is_nan(value), literal
			} else {
				sign := if literal.starts_with('-') { -1 } else { 1 }
				assert math.is_inf(value, sign), literal
			}
		}
	}
}
