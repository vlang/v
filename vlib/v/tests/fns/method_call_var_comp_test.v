struct EmptyStruct {
	a int
	b []int
}

struct Encoder {
}

fn (e &Encoder) encode_array[T](val []T) []T {
	return val
}

fn encode_struct[T](val T) []string {
	e := Encoder{}
	mut out := []string{}
	$if T is $struct {
		$for field in T.fields {
			out << field.name
			out << field.is_array.str()

			$if field.is_array {
				value := val.$(field.name)
				out << e.encode_array(value).str()
				out << e.encode_array([1, 2]).str()
			}
		}
	}
	return out
}

fn test_main() {
	out := encode_struct(EmptyStruct{3, [2, 1]})
	assert out[0] == 'a'
	assert out[1] == 'false'
	assert out[2] == 'b'
	assert out[3] == 'true'
	assert out[4] == '[2, 1]'
	assert out[5] == '[1, 2]'
}
