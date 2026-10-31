// vtest vflags: -w
import json2

pub struct User {
	name   string
	age    int
	height f64
}

type Users = map[string]User

const json_users = '{
	"tom": { "name": "Tom", "age": 45, "height": 1.97 },
	"martin": { "name": "Martin", "age": 40, "height": 1.8 }
}'

fn test_alias_with_map() {
	a := json2.decode[map[string]User{}](json_users)!
	b := json2.decode[Users](json_users)!

	assert Users(a) == b

	c := json2.encode(a, escape_unicode: true)
	d := json2.encode(b, escape_unicode: true)

	assert c == d
}
