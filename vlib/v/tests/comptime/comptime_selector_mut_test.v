struct Struct {
mut:
	a string
	b int
}

fn decode_string[T](mut value T) {
	value = 'gg'
}

fn decode_string_int[T](mut value T) {
	value = 123
}

fn decode[T]() T {
	key := 'a'
	mut result := T{}
	$for field in T.fields {
		$if field.typ is string {
			decode_string(mut result.$(field.name))
		} $else $if field.typ is int {
			decode_string_int(mut result.$(field.name))
		}
	}
	return result
}

fn test_main() {
	assert decode[Struct]() == Struct{
		a: 'gg'
		b: 123
	}
}

struct OptionStruct {
mut:
	v ?int
}

fn decode_option_int[T](mut value T) {
	$if T is int {
		value = 42
	}
}

fn decode_option[T]() T {
	mut result := T{}
	$for field in T.fields {
		$if field.typ is $option {
			decode_option_int(mut result.$(field.name) ?)
		}
	}
	return result
}

fn test_comptime_option_payload_mut_generic() {
	result := decode_option[OptionStruct]()
	assert result.v? == 42
}
