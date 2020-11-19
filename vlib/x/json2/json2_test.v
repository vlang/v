import x.json2

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
	name   string
	age    int
	salary f32
	title  JobTitle
}

fn (e Employee) to_json() string {
	mut mp := map[string]json2.Any
	mp['name'] = e.name
	mp['age'] = e.age
	mp['salary'] = e.salary
	mp['title'] = int(e.title)

	/*
	$for field in Employee.fields {
		d := e.$(field.name)

		$if field.typ is JobTitle {
			mp[field.name] = json.encode<int>(d)
		} $else {
			mp[field.name] = d
		}
	}
	*/

	return mp.str()
}

fn test_simple() {
	x := Employee{'Peter', 28, 95000.5, .worker}
	s := json2.encode<Employee>(x)
	eprintln('Employee x: $s')
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	/*
	y := json.decode(Employee, s) or {
		assert false
		Employee{}
	}*/
	y := json2.raw_decode(s) or {
		assert false
		json2.Any{}
	}
	eprintln('Employee y: $y')
	ym := y.as_map()
	assert ym['name'].str() == 'Peter'
	assert ym['age'].int() == 28
	assert ym['salary'].f64() == 95000.5
	// assert y['title'] == .worker
	assert ym['title'].int() == 2
}

fn test_fast_raw_decode() {
	s := '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	o := json2.fast_raw_decode(s) or {
		assert false
		json2.Any{}
	}
	str := o.str()

	assert str == '{"name":"Peter","age":"28","salary":"95000.5","title":"2"}'
}

fn test_character_unescape() {
	// Need to test `\r`, `\b`, `\f` ??
	message := '{
		"newline":"new\\nline", 
		"tab":"\\ttab", 
		"backslash": "back\\\\slash", 
		"quotes": "\\"quotes\\"", 
		"slash":"\/dev\/null"
	}'
	mut obj := json2.raw_decode(message) or {
		assert false
		json2.Any{}
	}
	lines := obj.as_map()
	eprintln("$lines")
	assert lines['newline'].str() == 'new\nline'
	assert lines['tab'].str() == '\ttab'
	assert lines['backslash'].str() == 'back\\slash'
	assert lines['quotes'].str() == '\"quotes\"'
	assert lines['slash'].str() == '/dev/null'
}

/*
struct User2 {
	age  int
	nums []int
}

struct User {
	age           int
	nums          []int
	last_name     string [json: lastName]
	is_registered bool   [json: IsRegistered]
	typ           int    [json: 'type']
	pets          string [raw; json: 'pet_animals']
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3], "type": 1, "lastName": "Johnson", "IsRegistered": true, "pet_animals": {"name": "Bob", "animal": "Dog"}}'
	u2 := json.decode(User2, s) or {
		exit(1)
	}
	println(u2)
	u := json.decode(User, s) or {
		exit(1)
	}
	println(u)
	assert u.age == 10
	assert u.last_name == 'Johnson'
	assert u.is_registered == true
	assert u.nums.len == 3
	assert u.nums[0] == 1
	assert u.nums[1] == 2
	assert u.nums[2] == 3
	assert u.typ == 1
	assert u.pets == '{"name":"Bob","animal":"Dog"}'
}

fn test_encode_user() {
	usr := User{
		age: 10
		nums: [1, 2, 3]
		last_name: 'Johnson'
		is_registered: true
		typ: 0
		pets: 'foo'
	}
	expected := '{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"foo"}'
	out := json.encode(usr)
	println(out)
	assert out == expected
}

struct Color {
	space string
	point string [raw]
}

fn test_raw_json_field() {
	color := json.decode(Color, '{"space": "YCbCr", "point": {"Y": 123}}') or {
		println('text')
		return
	}
	assert color.point == '{"Y":123}'
	assert color.space == 'YCbCr'
}

struct City {
	name string
}

struct Country {
	cities []City
	name   string
}

fn test_struct_in_struct() {
	country := json.decode(Country, '{ "name": "UK", "cities": [{"name":"London"}, {"name":"Manchester"}]}') or {
		assert false
		exit(1)
	}
	assert country.name == 'UK'
	assert country.cities.len == 2
	assert country.cities[0].name == 'London'
	assert country.cities[1].name == 'Manchester'
	println(country.cities)
}
*/

fn test_encode_map() {
	expected := '{"one":1,"two":2,"three":3,"four":4}'
	numbers := {
		'one': json2.Any(1)
		'two': json2.Any(2)
		'three': json2.Any(3)
		'four': json2.Any(4)
	}
	out := numbers.str()
	// out := json.encode(numbers)

	println(out)
	assert out == expected
}
/*
fn test_parse_map() {
	expected := {
		'one': 1
		'two': 2
		'three': 3
		'four': 4
	}
	out := json.decode<map[string]int>('{"one":1,"two":2,"three":3,"four":4}') or {
		assert false
		r := {
			'': 0
		}
		r
	}
	println(out)
	assert out == expected
}

struct Data {
	countries []Country
	users     map[string]User
	extra     map[string]map[string]int
}

fn test_nested_type() {
	data_expected := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'

	data := Data{
		countries: [
			Country{
				name: 'UK'
				cities: [City{'London'},
					City{'Manchester'},
				]
			},
			Country{
				name: 'KU'
				cities: [City{'Donlon'},
					City{'Termanches'},
				]
			},
		]
		users: {
			'Foo': User{
				age: 10
				nums: [1, 2, 3]
				last_name: 'Johnson'
				is_registered: true
				typ: 0
				pets: 'little foo'
			},
			'Boo': User{
				age: 20
				nums: [5, 3, 1]
				last_name: 'Smith'
				is_registered: false
				typ: 4
				pets: 'little boo'
			}
		},
		extra: {
			'2': {
				'n1': 2
				'n2': 4
				'n3': 8
				'n4': 16
			},
			'3': {
				'n1': 3
				'n2': 9
				'n3': 27
				'n4': 81
			},
		}
	}
	out := json.encode(data)
	println(out)
	assert out == data_expected

	data2 := json.decode(Data, data_expected) or {
		assert false
		Data{}
	}
	assert data2.countries.len == data.countries.len
	for i in 0..1 {
		assert data2.countries[i].name == data.countries[i].name
		assert data2.countries[i].cities.len == data.countries[i].cities.len
		for j in 0..1 {
			assert data2.countries[i].cities[j].name == data.countries[i].cities[j].name
		}
	}

	for key, user in data.users {
		assert data2.users[key].age == user.age
		assert data2.users[key].nums == user.nums
		assert data2.users[key].last_name == user.last_name
		assert data2.users[key].is_registered == user.is_registered
		assert data2.users[key].typ == user.typ
		// assert data2.users[key].pets == user.pets // TODO FIX
	}

	for k, v in data.extra {
		for k2, v2 in v {
			assert data2.extra[k][k2] == v2
		}
	}
}

fn test_errors() {
	invalid_array := fn () {
		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":{"name":"Donlon"},"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'

		json.decode(Data, data) or {
			println(err)
			assert err.starts_with('Json element is not an array:')
			return
		}
		assert false
	}
	invalid_object := fn() {
		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":[{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}],"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'

		json.decode(Data, data) or {
			println(err)
			assert err.starts_with('Json element is not an object:')
			return
		}
		assert false
	}
	invalid_array()
	invalid_object()
}
*/
