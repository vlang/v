struct Encoder {}

struct StructType[T] {
mut:
	val T
}

fn (e &Encoder) encode_struct[U](val U) ! {
	$for field in U.fields {
		value := val.$(field.name)
		$if field.typ is $struct {
			e.encode_struct(value)!
		} $else $if field.typ is $map {
			e.encode_map(value)!
		}
	}
}

fn (e &Encoder) encode_map[U](val U) ! {
	for k, v in val {
		e.encode_value_with_level(v)!
	}
}

fn (e &Encoder) encode_value_with_level[U](val U) ! {
	$if U is $struct {
		e.encode_struct(val)!
	} $else $if U is $map {
		e.encode_map(val)!
	}
}

fn test_simple_cases() {
	e := Encoder{}
	e.encode_struct(StructType[map[string]string]{
		val: {
			'1': '1'
		}
	})!
	e.encode_struct(StructType[map[string]map[string]int]{})!
	e.encode_struct(StructType[map[string]map[string]int]{
		val: {
			'a': {
				'1': 1
			}
		}
	})!

	assert true
}
