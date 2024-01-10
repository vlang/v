struct IntArray {
	a []int
}

fn encode_array[U](val []U) []string {
	mut out := []string{}
	$if U is int {
		out << 'is int'
	} $else $if U is $struct {
		out << 'is struct'
	}
	return out
}

fn encode_struct[U](val U) ?[]string {
	$for field in U.fields {
		if field.is_array {
			value := val.$(field.name)
			return encode_array(value)
		}
	}
	return none
}

fn test_main() {
	int_array := IntArray{
		a: [1, 2, 3]
	}
	assert encode_struct(int_array)?.str() == "['is int']"
}
