import x.json2

fn maybe_map_map[T, U, X, Y](m map[T]U, f fn (T, U) !(X, Y)) !map[X]Y {
	mut r := map[X]Y{}
	for k, v in m {
		nk, nv := f(k, v)!
		r[nk] = nv
	}
	return r
}

type Snowflake = int

struct User {
	id   Snowflake
	name string
}

fn User.parse(j json2.Any) !User {
	match j {
		map[string]json2.Any {
			return User{
				id:   j['id']! as int
				name: j['name']! as string
			}
		}
		else {
			return error('expected user to be object, got ${j.type_name()}')
		}
	}
}

struct Resolved {
	users ?map[Snowflake]User
}

fn Resolved.parse(j json2.Any) !Resolved {
	match j {
		map[string]json2.Any {
			return Resolved{
				users: if m := j['users'] {
					maybe_map_map[string, json2.Any, Snowflake, User](m as map[string]json2.Any,
						fn (k string, v json2.Any) !(Snowflake, User) {
						return Snowflake(k.int()), User.parse(v)!
					})!
				} else {
					none
				}
			}
		}
		else {
			panic('asdadsd')
		}
	}
}

fn test_main() {
	a := Resolved.parse({
		'0':  json2.Any('1')
		'2':  '3'
		'a4': 5
		'6':  '7'
	}) or {
		assert err.msg() == 'expected user to be object, got string'
		return
	}
	assert a.users == none
}
