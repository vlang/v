struct IntArray {
	b []Ab
}

struct Ab {
	a []int
}

fn encode_struct[T](val T) []string {
	mut out := []string{}
	$if T is $Struct {
		$for field in T.fields {
			value := val.$(field.name)
			out << typeof(value).idx.str()
			out << typeof(value).name
		}
	}
	return out
}

fn encode_array(val []Ab) {
	encode_struct(val[0])
}

fn test_main() {
	int_array := IntArray{
		b: [Ab{[2]}, Ab{[3]}]
	}
	mut out := encode_struct(int_array)
	assert out[0] == typeof[[]Ab]().idx.str()
	assert out[1] == typeof[[]Ab]().name
}
