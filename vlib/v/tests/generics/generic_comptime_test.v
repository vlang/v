struct EmptyStruct {
	b []int = [1, 2, 3]
}

fn encode_array[T](val []T) []int {
	return val
}

fn encode_struct[T](val T) []int {
	$if T is $struct {
		$for field in T.fields {
			$if field.is_array {
				return encode_array(val.$(field.name))
			}
		}
	}
	return [0]
}

fn encoder() []int {
	return encode_struct(EmptyStruct{})
}

fn test_main() {
	assert encoder() == [1, 2, 3]
}
