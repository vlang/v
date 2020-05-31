import json

struct Employee {
	name string
	age  int
	salary f32
}

fn test_simple() {
	x := Employee{'Peter', 28, 95000.5}
	s := json.encode(x)
	eprintln('Employee x: $s')
	assert s == '{"name":"Peter","age":28,"salary":95000.5}'
	y := json.decode(Employee, s) or {
		assert false
		Employee{}
	}
	eprintln('Employee y: $y')
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 95000.5
}

struct User2 {
	age  int
	nums []int
}

struct User {
	age           int
	nums          []int
	last_name     string [json:lastName]
	is_registered bool   [json:IsRegistered]
	typ           int    [json:'type']
	pets          string [raw; json:'pet_animals']
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
