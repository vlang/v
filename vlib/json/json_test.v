import json
import time

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

fn test_simple() {
	x := Employee{'Peter', 28, 95000.5, .worker}
	s := json.encode(x)
	// eprintln('Employee x: $s')
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	y := json.decode(Employee, s)!
	// eprintln('Employee y: $y')
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 95000.5
	assert y.title == .worker
}

const currency_id = 'cconst'

struct Price {
	net         f64
	currency_id string [json: currencyId] = currency_id
}

fn test_field_with_default_expr() {
	data := '[{"net":1},{"net":2,"currencyId":"cjson"}]'
	prices := json.decode([]Price, data)!
	assert prices == [Price{
		net: 1
		currency_id: 'cconst'
	}, Price{
		net: 2
		currency_id: 'cjson'
	}]
}

fn test_decode_top_level_array() {
	s := '[{"name":"Peter", "age": 29}, {"name":"Bob", "age":31}]'
	x := json.decode([]Employee, s) or { panic(err) }
	assert x.len == 2
	assert x[0].name == 'Peter'
	assert x[0].age == 29
	assert x[1].name == 'Bob'
	assert x[1].age == 31
}

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

fn test_encode_decode_sumtype() {
	t := time.now()
	game := SomeGame{
		title: 'Super Mega Game'
		player: Human{'Monke'}
		other: [
			Entity(Item{'Pen'}),
			Item{'Cookie'},
			Animal.cat,
			'Stool',
			t,
		]
	}
	// eprintln('Game: $game')

	enc := json.encode(game)
	// eprintln('Encoded Game: $enc')

	assert enc == '{"title":"Super Mega Game","player":{"name":"Monke","_type":"Human"},"other":[{"tag":"Pen","_type":"Item"},{"tag":"Cookie","_type":"Item"},1,"Stool",{"_type":"Time","value":$t.unix_time()}]}'

	dec := json.decode(SomeGame, enc)!
	// eprintln('Decoded Game: $dec')

	assert game.title == dec.title
	assert game.player == dec.player
	assert (game.other[2] as Animal) == (dec.other[2] as Animal)
	assert (game.other[4] as time.Time).unix_time() == (dec.other[4] as time.Time).unix_time()
}

fn bar<T>(payload string) !Bar { // ?T doesn't work currently
	result := json.decode(T, payload)!
	return result
}

struct Bar {
	x string
}

fn test_generic() {
	result := bar<Bar>('{"x":"test"}') or { Bar{} }
	assert result.x == 'test'
}

struct User2 {
	age      int
	nums     []int
	reg_date time.Time
}

struct User {
	age           int
	nums          []int
	last_name     string [json: lastName]
	is_registered bool   [json: IsRegistered]
	typ           int    [json: 'type']
	pets          string [json: 'pet_animals'; raw]
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3], "type": 1, "lastName": "Johnson", "IsRegistered": true, "pet_animals": {"name": "Bob", "animal": "Dog"}}'
	u2 := json.decode(User2, s)!
	// println(u2)
	u := json.decode(User, s)!
	// println(u)
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

fn test_encode_decode_time() {
	user := User2{
		age: 25
		reg_date: time.new_time(year: 2020, month: 12, day: 22, hour: 7, minute: 23)
	}
	s := json.encode(user)
	// println(s)
	assert s.contains('"reg_date":1608621780')
	user2 := json.decode(User2, s)!
	assert user2.reg_date.str() == '2020-12-22 07:23:00'
	// println(user2)
	// println(user2.reg_date)
}

fn (mut u User) foo() string {
	return json.encode(u)
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
	out := json.encode(usr)
	// println(out)
	assert out == expected
	// Test json.encode on mutable pointers
	assert usr.foo() == expected
}

struct Color {
	space string
	point string [raw]
}

fn test_raw_json_field() {
	color := json.decode(Color, '{"space": "YCbCr", "point": {"Y": 123}}') or {
		// println('text')
		return
	}
	assert color.point == '{"Y":123}'
	assert color.space == 'YCbCr'
}

fn test_bad_raw_json_field() {
	color := json.decode(Color, '{"space": "YCbCr"}') or {
		// println('text')
		return
	}
	assert color.point == ''
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
	country := json.decode(Country, '{ "name": "UK", "cities": [{"name":"London"}, {"name":"Manchester"}]}')!
	assert country.name == 'UK'
	assert country.cities.len == 2
	assert country.cities[0].name == 'London'
	assert country.cities[1].name == 'Manchester'
	// println(country.cities)
}

fn test_encode_map() {
	expected := '{"one":1,"two":2,"three":3,"four":4}'
	numbers := {
		'one':   1
		'two':   2
		'three': 3
		'four':  4
	}
	out := json.encode(numbers)
	// println(out)
	assert out == expected
}

fn test_parse_map() {
	expected := {
		'one':   1
		'two':   2
		'three': 3
		'four':  4
	}
	out := json.decode(map[string]int, '{"one":1,"two":2,"three":3,"four":4}')!
	// println(out)
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
				cities: [City{'London'}, City{'Manchester'}]
			},
			Country{
				name: 'KU'
				cities: [City{'Donlon'}, City{'Termanches'}]
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
			}
			'Boo': User{
				age: 20
				nums: [5, 3, 1]
				last_name: 'Smith'
				is_registered: false
				typ: 4
				pets: 'little boo'
			}
		}
		extra: {
			'2': {
				'n1': 2
				'n2': 4
				'n3': 8
				'n4': 16
			}
			'3': {
				'n1': 3
				'n2': 9
				'n3': 27
				'n4': 81
			}
		}
	}
	out := json.encode(data)
	// println(out)
	assert out == data_expected
	data2 := json.decode(Data, data_expected)!
	assert data2.countries.len == data.countries.len
	for i in 0 .. 1 {
		assert data2.countries[i].name == data.countries[i].name
		assert data2.countries[i].cities.len == data.countries[i].cities.len
		for j in 0 .. 1 {
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

struct Foo<T> {
pub:
	name string
	data T
}

fn test_generic_struct() {
	foo_int := Foo<int>{'bar', 12}
	foo_enc := json.encode(foo_int)
	assert foo_enc == '{"name":"bar","data":12}'
	foo_dec := json.decode(Foo<int>, foo_enc)!
	assert foo_dec.name == 'bar'
	assert foo_dec.data == 12
}

fn test_errors() {
	invalid_array := fn () {
		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":{"name":"Donlon"},"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'
		json.decode(Data, data) or {
			// println(err)
			assert err.msg().starts_with('Json element is not an array:')
			return
		}
		assert false
	}
	invalid_object := fn () {
		data := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":[{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}],"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'
		json.decode(Data, data) or {
			// println(err)
			assert err.msg().starts_with('Json element is not an object:')
			return
		}
		assert false
	}
	invalid_array()
	invalid_object()
}

type ID = string

struct Message {
	id ID
}

fn test_decode_alias_struct() {
	msg := json.decode(Message, '{"id": "118499178790780929"}')!
	// hacky way of comparing aliased strings
	assert msg.id.str() == '118499178790780929'
}

fn test_encode_alias_struct() {
	expected := '{"id":"118499178790780929"}'
	msg := Message{'118499178790780929'}
	out := json.encode(msg)
	assert out == expected
}

struct List {
	id    int
	items []string
}

fn test_list() {
	list := json.decode(List, '{"id": 1, "items": ["1", "2"]}')!
	assert list.id == 1
	assert list.items == ['1', '2']
}

fn test_list_no_id() {
	list := json.decode(List, '{"items": ["1", "2"]}')!
	assert list.id == 0
	assert list.items == ['1', '2']
}

fn test_list_no_items() {
	list := json.decode(List, '{"id": 1}')!
	assert list.id == 1
	assert list.items == []
}

struct Info {
	id    int
	items []string
	maps  map[string]string
}

fn test_decode_null_object() {
	info := json.decode(Info, '{"id": 22, "items": null, "maps": null}')!
	assert info.id == 22
	assert '$info.items' == '[]'
	assert '$info.maps' == '{}'
}

fn test_decode_missing_maps_field() {
	info := json.decode(Info, '{"id": 22, "items": null}')!
	assert info.id == 22
	assert '$info.items' == '[]'
	assert '$info.maps' == '{}'
}

struct Foo2 {
	name string
}

fn test_pretty() {
	foo := Foo2{'Bob'}
	assert json.encode_pretty(foo) == '{
	"name":	"Bob"
}'
}

struct Foo3 {
	name string
	age  int    [omitempty]
}

fn test_omit_empty() {
	foo := Foo3{'Bob', 0}
	assert json.encode_pretty(foo) == '{
	"name":	"Bob"
}'
	// println('omitempty:')
	// println(json.encode_pretty(foo))
}

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
	return json.encode(data)
}

fn test_encode_sumtype_defined_ahead() {
	ret := create_game_packet(&GamePacketData(GPScale{}))
	// println(ret)
	assert ret == '{"value":0,"_type":"GPScale"}'
}

struct StByteArray {
	ba []byte
}

fn test_byte_array() {
	assert json.encode(StByteArray{ ba: [byte(1), 2, 3, 4, 5] }) == '{"ba":[1,2,3,4,5]}'
}

struct Aa {
	sub AliasType
}

struct Bb {
	a int
}

type AliasType = Bb

fn test_encode_alias_field() {
	s := json.encode(Aa{
		sub: Bb{
			a: 1
		}
	})
	println(s)
	assert s == '{"sub":{"a":1}}'
}

//

struct APrice {}

struct Association {
	association &Association = unsafe { nil }
	price       APrice
}

fn test_encoding_struct_with_pointers() {
	value := Association{
		association: &Association{
			price: APrice{}
		}
		price: APrice{}
	}
	assert json.encode(value) == '{"association":{"price":{}},"price":{}}'
}
