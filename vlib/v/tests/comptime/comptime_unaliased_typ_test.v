type StringAlias = string
type BoolAlias = bool
type IntAlias = int

struct Decoder {
	json string
}

pub fn decode[T](val string) !T {
	mut decoder := Decoder{
		json: val
	}

	mut result := T{}
	decoder.decode_value(mut result)!
	return result
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is string {
	} $else $if T.unaliased_typ in [$int, $float, $enum] {
		bytes := unsafe { decoder.json.str.vbytes(decoder.json.len) }
		unsafe {
			string_buffer_to_generic_number(mut val, bytes)
		}
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

@[direct_array_access; unsafe]
pub fn string_buffer_to_generic_number[T](mut result T, data []u8) {
	$if T is $option {
	} $else $if T.unaliased_typ is string {
		panic('should not happens')
	} $else $if T.unaliased_typ is $int {
		mut is_negative := false
		for ch in data {
			if ch == `-` {
				is_negative = true
				continue
			}
			digit := T(ch - `0`)
			result = T(*result * 10 + digit)
		}
		if is_negative {
			result *= -1
		}
	} $else {
		panic('unsupported type ${typeof[T]().name}')
	}
}

fn test_main() {
	value := '1234567890123456789'
	_ := decode[int](value)!
	_ := decode[IntAlias](value)!
	_ := decode[StringAlias]('"abcd"')!
	_ := decode[int](value)!
	_ := decode[string]('"abcd"')!

	assert true
}
