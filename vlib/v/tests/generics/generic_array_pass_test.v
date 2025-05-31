struct Decoder {}

pub fn decode[T](val string) !T {
	mut decoder := Decoder{}
	mut result := T{}
	decoder.decode_value(mut result)!
	return result
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is $array {
		decoder.decode_array(mut val)!
		return
	} $else $if T.unaliased_typ is $struct {
		$for field in T.fields {
			decoder.decode_value(mut val.$(field.name))!
		}
	}
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {}

struct Foo {
	int  []int
	oint []?int
}

fn test_main() {
	decode[Foo]('')!
}
