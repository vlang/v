import x.json2
import time

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
pub mut:
	name   string
	age    int
	salary f32
	title  JobTitle
}

struct EmployeeOp {
pub mut:
	name      ?string = none
	last_name ?string = none
	age       ?int
	salary    f32
	title     JobTitle
}

fn (e Employee) to_json() string {
	mut mp := map[string]json2.Any{}
	mp['name'] = json2.Any(e.name)
	mp['age'] = json2.Any(e.age)
	mp['salary'] = json2.Any(e.salary)
	mp['title'] = json2.Any(int(e.title))
	/*
	$for field in Employee.fields {
		d := e.$(field.name)

		$if field.typ is JobTitle {
			mp[field.name] = json2.encode<int>(d)
		} $else {
			mp[field.name] = d
		}
	}
	*/
	return mp.str()
}

fn (mut e Employee) from_json(any json2.Any) {
	mp := any.as_map()
	e.name = mp['name'] or { json2.Any('') }.str()
	e.age = mp['age'] or { json2.Any(0) }.int()
	e.salary = mp['salary'] or { json2.Any(0) }.f32()
	e.title = unsafe { JobTitle(mp['title'] or { json2.Any(0) }.int()) }
}

fn test_simple() {
	x := Employee{'Peter', 28, 95000.5, .worker}
	s := json2.encode<Employee>(x)
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	y := json2.decode<Employee>(s) or {
		println(err)
		assert false
		return
	}
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 95000.5
	assert y.title == .worker
}

// ! BUGFIX
// fn test_simplegg() {
// 	// x := EmployeeOp{'Peter', 28, 95000.5, .worker}
// 	x := EmployeeOp{
// 		name: 'vshfvhsd'
// 	}
// 	s := json2.encode<EmployeeOp>(x)
// 	assert s == '{"name":"vshfvhsd","age":0,"salary":0.0,"title":0.0}'
// 	// y := json2.decode<EmployeeOp>(s) or {
// 	// 	println(err)
// 	// 	assert false
// 	// 	return
// 	// }
// 	// assert y.name == 'Peter'
// 	// assert y.age == 28
// 	// assert y.salary == 95000.5
// 	// assert y.title == .worker
// }

const currency_id = 'cconst'

struct Price {
	net         f64
	currency_id string [json: currencyId] = currency_id
}

fn test_fast_raw_decode() {
	s := '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	o := json2.fast_raw_decode(s) or {
		assert false
		json2.Any(json2.null)
	}
	str := o.str()
	assert str == '{"name":"Peter","age":"28","salary":"95000.5","title":"2"}'
}

fn test_character_unescape() {
	message := r'{
	"newline": "new\nline",
	"tab": "\ttab",
	"backslash": "back\\slash",
	"quotes": "\"quotes\"",
	"slash":"\/dev\/null"
}'
	mut obj := json2.raw_decode(message) or {
		println(err)
		assert false
		return
	}
	lines := obj.as_map()
	assert lines['newline'] or { 0 }.str() == 'new\nline'
	assert lines['tab'] or { 0 }.str() == '\ttab'
	assert lines['backslash'] or { 0 }.str() == 'back\\slash'
	assert lines['quotes'] or { 0 }.str() == '"quotes"'
	assert lines['slash'] or { 0 }.str() == '/dev/null'
}

fn (mut u User2) from_json(an json2.Any) {
	mp := an.as_map()
	mut js_field_name := ''
	$for field in User.fields {
		js_field_name = field.name
		for attr in field.attrs {
			if attr.starts_with('json:') {
				js_field_name = attr.all_after('json:').trim_left(' ')
				break
			}
		}
		match field.name {
			'age' { u.age = mp[js_field_name] or { 0 }.int() }
			'nums' { u.nums = mp[js_field_name] or { 0 }.arr().map(it.int()) }
			else {}
		}
	}
}

// ! BUGFIX - .from_json(res)
// fn test_field_with_default_expr() {
// 	data := '[{"net":1},{"net":2,"currencyId":"cjson"}]'
// 	prices := json2.decode<[]Price>(data)!
// 	assert prices == [Price{
// 		net: 1
// 		currency_id: 'cconst'
// 	}, Price{
// 		net: 2
// 		currency_id: 'cjson'
// 	}]
// }

// ! BUGFIX - .from_json(res)
// fn test_decode_top_level_array() {
// 	s := '[{"name":"Peter", "age": 29}, {"name":"Bob", "age":31}]'
// 	x := json2.decode<[]Employee>(s) or { panic(err) }
// 	assert x.len == 2
// 	assert x[0].name == 'Peter'
// 	assert x[0].age == 29
// 	assert x[1].name == 'Bob'
// 	assert x[1].age == 31
// }

struct Human {
	name string
}

struct Item {
	tag string
}

enum Animal {
	dog // Will be encoded as `0`
	cat
}

type Entity = Animal | Human | Item | string | time.Time

struct SomeGame {
	title  string
	player Entity
	other  []Entity
}

// ! BUGFIX - .from_json(res)
// fn test_encode_decode_sumtype() {
// 	t := time.now()
// 	game := SomeGame{
// 		title: 'Super Mega Game'
// 		player: Human{'Monke'}
// 		other: [
// 			Entity(Item{'Pen'}),
// 			Item{'Cookie'},
// 			Animal.cat,
// 			'Stool',
// 			t,
// 		]
// 	}
// 	// eprintln('Game: $game')

// 	enc := json2.encode(game)
// 	// eprintln('Encoded Game: $enc')

// 	assert enc == '{"title":"Super Mega Game","player":{"name":"Monke","_type":"Human"},"other":[{"tag":"Pen","_type":"Item"},{"tag":"Cookie","_type":"Item"},1,"Stool",{"_type":"Time","value":${t.unix_time()}}]}'

// 	dec := json2.decode<SomeGame>(enc)!
// 	// eprintln('Decoded Game: $dec')

// 	assert game.title == dec.title
// 	assert game.player == dec.player
// 	assert (game.other[2] as Animal) == (dec.other[2] as Animal)
// 	assert (game.other[4] as time.Time).unix_time() == (dec.other[4] as time.Time).unix_time()
// }

fn bar<T>(payload string) !Bar { // ?T doesn't work currently
	result := json2.decode<T>(payload)!
	return result
}

struct Bar {
	x string
}

// ! BUGFIX - .from_json(res)
// fn test_generic() {
// 	result := bar<Bar>('{"x":"test"}') or { Bar{} }
// 	assert result.x == 'test'
// }

struct User2 {
mut:
	age      int
	nums     []int
	reg_date time.Time
}

// User struct needs to be `pub mut` for now in order to access and manipulate values
struct User {
pub mut:
	age           int
	nums          []int
	last_name     string [json: lastName]
	is_registered bool   [json: IsRegistered]
	typ           int    [json: 'type']
	pets          string [json: 'pet_animals'; raw]
}

fn (mut u User) from_json(an json2.Any) {
	mp := an.as_map()
	mut js_field_name := ''
	$for field in User.fields {
		// FIXME: C error when initializing js_field_name inside comptime for
		js_field_name = field.name
		for attr in field.attrs {
			if attr.starts_with('json:') {
				js_field_name = attr.all_after('json:').trim_left(' ')
				break
			}
		}
		match field.name {
			'age' { u.age = mp[js_field_name] or { 0 }.int() }
			'nums' { u.nums = mp[js_field_name] or { 0 }.arr().map(it.int()) }
			'last_name' { u.last_name = mp[js_field_name] or { 0 }.str() }
			'is_registered' { u.is_registered = mp[js_field_name] or { 0 }.bool() }
			'typ' { u.typ = mp[js_field_name] or { 0 }.int() }
			'pets' { u.pets = mp[js_field_name] or { 0 }.str() }
			else {}
		}
	}
}

fn (u User) to_json() string {
	// TODO: derive from field
	mut mp := {
		'age': json2.Any(u.age)
	}
	mp['nums'] = u.nums.map(json2.Any(it))
	mp['lastName'] = json2.Any(u.last_name)
	mp['IsRegistered'] = json2.Any(u.is_registered)
	mp['type'] = json2.Any(u.typ)
	mp['pet_animals'] = json2.Any(u.pets)
	return mp.str()
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3], "type": 1, "lastName": "Johnson", "IsRegistered": true, "pet_animals": {"name": "Bob", "animal": "Dog"}}'
	u2 := json2.decode<User2>(s)!
	u := json2.decode<User>(s)!
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

// ! BUGFIX - .from_json(res)
// fn test_encode_decode_time() {
// 	user := User2{
// 		age: 25
// 		reg_date: time.new_time(year: 2020, month: 12, day: 22, hour: 7, minute: 23)
// 	}
// 	s := json2.encode(user)
// 	// println(s) //{"age":25,"nums":[],"reg_date":{"year":2020,"month":12,"day":22,"hour":7,"minute":23,"second":0,"microsecond":0,"unix":1608621780,"is_local":false}}
// 	assert s.contains('"reg_date":1608621780')
// 	user2 := json2.decode<User2>(s)!
// 	assert user2.reg_date.str() == '2020-12-22 07:23:00'
// 	// println(user2)
// 	// println(user2.reg_date)
// }

fn (mut u User) foo() string {
	return json2.encode(u)
}

fn test_encode_user() {
	mut usr := User{
		age: 10
		nums: [1, 2, 3]
		last_name: 'Johnson'
		is_registered: true
		typ: 0
		pets: 'foo'
	}
	expected := '{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"foo"}'
	out := json2.encode<User>(usr)
	assert out == expected
	// Test json2.encode on mutable pointers
	assert usr.foo() == expected
}

struct Color {
pub mut:
	space string
	point string [raw]
}

fn (mut c Color) from_json(an json2.Any) {
	mp := an.as_map()
	$for field in Color.fields {
		match field.name {
			'space' { c.space = mp[field.name] or { 0 }.str() }
			'point' { c.point = mp[field.name] or { 0 }.str() }
			else {}
		}
	}
}

fn test_raw_json_field() {
	color := json2.decode<Color>('{"space": "YCbCr", "point": {"Y": 123}}') or {
		assert false
		Color{}
	}
	assert color.point == '{"Y":123}'
	assert color.space == 'YCbCr'
}

// ! FIX: returning 0
// fn test_bad_raw_json_field() {
// 	color := json2.decode<Color>('{"space": "YCbCr"}') or {
// 		// println('text')
// 		return
// 	}
// 	assert color.point == ''
// 	assert color.space == 'YCbCr'
// }

struct City {
	name string
}

struct Country {
	cities []City
	name   string
}

// ! BUGFIX - .from_json(res)
// fn test_struct_in_struct() {
// 	country := json2.decode<Country>('{ "name": "UK", "cities": [{"name":"London"}, {"name":"Manchester"}]}')!
// 	assert country.name == 'UK'
// 	assert country.cities.len == 2
// 	assert country.cities[0].name == 'London'
// 	assert country.cities[1].name == 'Manchester'
// 	// println(country.cities)
// }
fn test_encode_map() {
	expected := '{"one":1,"two":2,"three":3,"four":4}'
	numbers := {
		'one':   json2.Any(1)
		'two':   json2.Any(2)
		'three': json2.Any(3)
		'four':  json2.Any(4)
	}
	// 	numbers := {
	// 		'one':   1
	// 		'two':   2
	// 		'three': 3
	// 		'four':  4
	// 	}
	// 	out := json2.encode(numbers)
	out := numbers.str()
	assert out == expected
}

// ! BUGFIX - .from_json(res)
// fn test_parse_map() {
// 	expected := {
// 		'one':   1
// 		'two':   2
// 		'three': 3
// 		'four':  4
// 	}
// 	out := json2.decode<map[string]int>('{"one":1,"two":2,"three":3,"four":4}')!
// 	// println(out)
// 	assert out == expected
// }

struct Data {
	countries []Country
	users     map[string]User
	extra     map[string]map[string]int
}

// ! BUGFIX - .from_json(res)
// fn test_nested_type() {
// 	data_expected := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'

// 	data := Data{
// 		countries: [
// 			Country{
// 				name: 'UK'
// 				cities: [City{'London'}, City{'Manchester'}]
// 			},
// 			Country{
// 				name: 'KU'
// 				cities: [City{'Donlon'}, City{'Termanches'}]
// 			},
// 		]
// 		users: {
// 			'Foo': User{
// 				age: 10
// 				nums: [1, 2, 3]
// 				last_name: 'Johnson'
// 				is_registered: true
// 				typ: 0
// 				pets: 'little foo'
// 			},
// 			'Boo': User{
// 				age: 20
// 				nums: [5, 3, 1]
// 				last_name: 'Smith'
// 				is_registered: false
// 				typ: 4
// 				pets: 'little boo'
// 			}
// 		},
// 		extra: {
// 			'2': {
// 				'n1': 2
// 				'n2': 4
// 				'n3': 8
// 				'n4': 16
// 			},
// 			'3': {
// 				'n1': 3
// 				'n2': 9
// 				'n3': 27
// 				'n4': 81
// 			},
// 		}
// 	}
// 	out := json2.encode(data)
// 	// println(out)
// 	assert out == data_expected
// 	data2 := json2.decode<Data>(data_expected)!
// 	assert data2.countries.len == data.countries.len
// 	for i in 0 .. 1 {
// 		assert data2.countries[i].name == data.countries[i].name
// 		assert data2.countries[i].cities.len == data.countries[i].cities.len
// 		for j in 0 .. 1 {
// 			assert data2.countries[i].cities[j].name == data.countries[i].cities[j].name
// 		}
// 	}

// 	for key, user in data.users {
// 		assert data2.users[key].age == user.age
// 		assert data2.users[key].nums == user.nums
// 		assert data2.users[key].last_name == user.last_name
// 		assert data2.users[key].is_registered == user.is_registered
// 		assert data2.users[key].typ == user.typ
// 		// assert data2.users[key].pets == user.pets // TODO FIX
// 	}

// 	for k, v in data.extra {
// 		for k2, v2 in v {
// 			assert data2.extra[k][k2] == v2
// 		}
// 	}
// }

// ! BUGFIX - .from_json(res)
// fn test_errors() {
// 	invalid_array := fn () {
// 		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":{"name":"Donlon"},"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'
// 		json2.decode<Data>(data) or {
// 			assert err.msg().starts_with('Json element is not an array:')
// 			return
// 		}
// 		assert false
// 	}
// 	invalid_object := fn () {
// 		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":[{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}],"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'
// 		json2.decode<Data>(data) or {
// 			assert err.msg().starts_with('Json element is not an object:')
// 			return
// 		}
// 		assert false
// 	}
// 	invalid_array()
// 	invalid_object()
// }

struct Foo<T> {
pub:
	name string
	data T
}

// ! BUGFIX - .from_json(res)
// fn test_generic_struct() {
// 	foo_int := Foo<int>{'bar', 12}
// 	foo_enc := json2.encode(foo_int)
// 	assert foo_enc == '{"name":"bar","data":12}'
// 	foo_dec := json2.decode<Foo<int>>(foo_enc)!
// 	assert foo_dec.name == 'bar'
// 	assert foo_dec.data == 12
// }

type ID = string
type GG = int

struct Message {
	id ID
	ij GG
}

// ! BUGFIX - .from_json(res)
// fn test_decode_alias_struct() {
// 	msg := json2.decode<Message>('{"id": "118499178790780929"}')!
// 	// hacky way of comparing aliased strings
// 	assert msg.id.str() == '118499178790780929'
// }

fn test_encode_alias_struct() {
	expected := '{"id":"118499178790780929","ij":999998888}'
	msg := Message{'118499178790780929', 999998888}
	out := json2.encode<Message>(msg)
	assert out == expected
}

struct List {
	id    int
	items []string
}

// ! BUGFIX - .from_json(res)
// fn test_list() {
// 	list := json2.decode<List>('{"id": 1, "items": ["1", "2"]}')!
// 	assert list.id == 1
// 	assert list.items == ['1', '2']
// }

// ! BUGFIX - .from_json(res)
// fn test_list_no_id() {
// 	list := json2.decode<List>('{"items": ["1", "2"]}')!
// 	assert list.id == 0
// 	assert list.items == ['1', '2']
// }

// ! BUGFIX - .from_json(res)
// fn test_list_no_items() {
// 	list := json2.decode<List>('{"id": 1}')!
// 	assert list.id == 1
// 	assert list.items == []
// }

struct Info {
	id    int
	items []string
	maps  map[string]string
}

// ! BUGFIX - .from_json(res)
// fn test_decode_null_object() {
// 	info := json2.decode<Info>('{"id": 22, "items": null, "maps": null}')!
// 	assert info.id == 22
// 	assert '${info.items}' == '[]'
// 	assert '${info.maps}' == '{}'
// }

// ! BUGFIX - .from_json(res)
// fn test_decode_missing_maps_field() {
// 	info := json2.decode<Info>('{"id": 22, "items": null}')!
// 	assert info.id == 22
// 	assert '${info.items}' == '[]'
// 	assert '${info.maps}' == '{}'
// }

struct Foo2 {
	name string
}

// ! BUGFIX - .from_json(res)
// fn test_pretty() {
// 	foo := Foo2{'Bob'}
// 	assert json2.encode_pretty(foo) == '{
// 	"name":	"Bob"
// }'
// }

struct Foo3 {
	name string
	age  int    [omitempty]
}

// ! BUGFIX - .from_json(res)
// fn test_omit_empty() {
// 	foo := Foo3{'Bob', 0}
// 	assert json2.encode_pretty(foo) == '{
// 	"name":	"Bob"
// }'
// 	// println('omitempty:')
// 	// println(json2.encode_pretty(foo))
// }

struct Asdasd {
	data GamePacketData
}

type GamePacketData = GPEquipItem | GPScale

struct GPScale {
	value f32
}

struct GPEquipItem {
	name string
}

fn create_game_packet(data &GamePacketData) string {
	return json2.encode(data)
}

// ! FIX: returning null
// fn test_encode_sumtype_defined_ahead() {
// 	ret := create_game_packet(&GamePacketData(GPScale{}))
// 	// println(ret)
// 	assert ret == '{"value":0,"_type":"GPScale"}'
// }

struct StByteArray {
	ba []byte
}

fn test_byte_array() {
	assert json2.encode(StByteArray{ ba: [byte(1), 2, 3, 4, 5] }) == '{"ba":[1,2,3,4,5]}'
}

struct Aa {
	sub AliasType
}

struct Bb {
	a int
}

type AliasType = Bb

// ! FIX: returning null
// fn test_encode_alias_field() {
// 	s := json2.encode(Aa{
// 		sub: Bb{
// 			a: 1
// 		}
// 	})
// 	assert s == '{"sub":{"a":1}}'
// }

struct APrice {}

struct Association {
	association &Association = unsafe { nil }
	price       APrice
}

// ! FIX: returning null
// fn test_encoding_struct_with_pointers() {
// 	value := Association{
// 		association: &Association{
// 			price: APrice{}
// 		}
// 		price: APrice{}
// 	}
// 	assert json2.encode(value) == '{"association":{"price":{}},"price":{}}'
// }
