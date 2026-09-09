pub struct User {
	name   string
	age    int
	height f64
}

type Users = map[string]User

struct Decoder {}

fn (mut decoder Decoder) decode_map[K, V](mut val map[K]V) {
	assert typeof(val).idx == typeof[map[string]User]().idx
}

fn (mut decoder Decoder) decode_value[T](mut val T) {
	$if T.unaliased_typ is $map {
		decoder.decode_map(mut val)
	}
}

pub fn decode[T]() T {
	mut decoder := Decoder{}
	mut result := T{}
	decoder.decode_value(mut result)
	return result
}

fn test_alias_generic_mut_reference_issue_24011() {
	users := decode[Users]()

	assert typeof(users).idx == typeof[Users]().idx
	assert users == Users(map[string]User{})
}
