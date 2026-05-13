struct User {
	name   string
	age    int
	height f64
}

type Users = map[string]User

struct Decoder {}

fn (mut decoder Decoder) decode_map[K, V](mut val map[K]V) ! {
	_ = decoder
	assert typeof[K]().idx == typeof[string]().idx
	assert typeof[V]().idx == typeof[User]().idx
	assert typeof(val).idx == typeof[map[string]User]().idx
}

fn decode[T](source string) !T {
	_ = source
	mut decoder := Decoder{}
	mut result := T{}
	decoder.decode_value(mut result)!
	return result
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is $map {
		decoder.decode_map(mut val)!
	}
}

fn test_alias_generic_mut_method_reference_issue_24012() {
	_ = decode[Users]('')!
	_ = decode[map[string]User]('')!
}
