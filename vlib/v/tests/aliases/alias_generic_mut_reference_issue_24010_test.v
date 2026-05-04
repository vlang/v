pub struct User {
	name   string
	age    int
	height f64
}

type Users = map[string]User

fn one() {
	assert typeof[Users]().unaliased_typ == typeof[map[string]User]().idx
}

fn two[T](mut mutable_val T, val T) {
	assert mutable_val == val
	assert T.idx == typeof(val).idx
	assert typeof(mutable_val).idx == typeof(val).idx
}

fn three[T, V](mut mutable_val T, val V) {
	assert mutable_val == val
	assert T.idx == typeof(val).idx
	assert V.idx == typeof(val).idx
	assert typeof(mutable_val).idx == typeof(val).idx
}

fn decode_map[K, V](mut val map[K]V) {
	assert typeof(val).idx == typeof[map[string]User]().idx
}

fn four[T](mut mutable_val T) {
	$if T.unaliased_typ is $map {
		decode_map(mut mutable_val)
	}
}

fn test_alias_generic_mut_reference_issue_24010() {
	mut mutable_users := Users(map[string]User{})
	users := Users(map[string]User{})

	one()
	two(mut mutable_users, users)
	three(mut mutable_users, users)
	four(mut mutable_users)
}
