fn encode[T](values T) string {
	$if T is $pointer {
		if voidptr(values) == unsafe { nil } {
			return 'nil'
		}
		return encode_pointer_struct(values)
	}
	return 'other'
}

fn encode_pointer_struct[T](values T) string {
	$for field in T.fields {
		dump('${field.name}') // works `name`
		mut val := values.$(field.name) // C error `root`
		dump('${val}')
	}
	return 'struct'
}

struct Node {
	name     string
	children []&Node
}

fn test_main() {
	assert encode('string') == 'other'
	assert encode(&Node(unsafe { nil })) == 'nil'
	assert encode(&Node{ name: 'root' }) == 'struct'
}
