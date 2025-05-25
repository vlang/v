import x.json2 as json
import time

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
pub mut:
	name         string
	age          int
	salary       f32
	title        JobTitle
	sub_employee SubEmployee //! FIXME - decode
}

pub struct SubEmployee {
pub mut:
	name   string
	age    int
	salary f32
	title  JobTitle
}

fn test_simple() {
	sub_employee := SubEmployee{
		name: 'João'
	}
	x := Employee{'Peter', 28, 95000.5, .worker, sub_employee}
	s := json.encode[Employee](x)
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2,"sub_employee":{"name":"João","age":0,"salary":0,"title":0}}'

	y := json.decode[Employee](s) or {
		println(err)
		assert false
		return
	}
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 95000.5
	assert y.title == .worker
	// assert y.sub_employee.name == 'João'
	assert y.sub_employee.age == 0
	assert y.sub_employee.salary == 0.0
	// assert y.sub_employee.title == .worker //! FIXME
}

// const currency_id = 'cconst'

// struct Price {
// 	net         f64
// 	currency_id string @[json: currencyId] = currency_id
// }

struct User2 {
mut:
	age      int
	nums     []int
	reg_date time.Time
}

// User struct needs to be `pub mut` for now in order to access and manipulate values
pub struct User {
pub mut:
	age           int
	nums          []int
	last_name     string @[json: lastName]
	is_registered bool   @[json: IsRegistered]
	typ           int    @[json: 'type']
	pets          string @[json: 'pet_animals'; raw]
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3], "type": 1, "lastName": "Johnson", "IsRegistered": true, "pet_animals": {"name": "Bob", "animal": "Dog"}}'

	u := json.decode[User](s)!

	assert u.age == 10
	assert u.last_name == 'Johnson'
	assert u.is_registered == true
	// assert u.nums.len == 3
	// assert u.nums[0] == 1
	// assert u.nums[1] == 2
	// assert u.nums[2] == 3
	assert u.typ == 1
	assert u.pets == '{"name":"Bob","animal":"Dog"}'
}

fn test_encode_decode_time() {
	user := User2{
		age:      25
		reg_date: time.new(year: 2020, month: 12, day: 22, hour: 7, minute: 23)
	}
	s := json.encode(user)
	assert s == '{"age":25,"nums":[],"reg_date":"2020-12-22T07:23:00.000Z"}'

	assert s.contains('"reg_date":"2020-12-22T07:23:00.000Z"')
	user2 := json.decode[User2](s)!
	assert user2.reg_date.str() == '2020-12-22 07:23:00'
}

fn (mut u User) foo() string {
	return json.encode(u)
}

fn test_encode_user() {
	mut usr := User{
		age:           10
		nums:          [1, 2, 3]
		last_name:     'Johnson'
		is_registered: true
		typ:           0
		pets:          'foo'
	}
	expected := '{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"foo"}'
	out := json.encode[User](usr)
	// println(out)
	assert out == expected
	// Test json.encode on mutable pointers
	assert usr.foo() == expected
}

struct Color {
pub mut:
	space string
	point string @[raw]
}

fn test_raw_json_field() {
	color := json.decode[Color]('{"space": "YCbCr", "point": {"Y": 123}}') or {
		assert false
		Color{}
	}
	assert color.point == '{"Y":123}'
	assert color.space == 'YCbCr'
}

fn test_bad_raw_json_field() {
	color := json.decode[Color]('{"space": "YCbCr"}') or { return }
	assert color.point == ''
	assert color.space == 'YCbCr'
}

fn test_encode_map() {
	expected := '{"one":1,"two":2,"three":3,"four":4}'
	numbers := {
		'one':   json.Any(1)
		'two':   json.Any(2)
		'three': json.Any(3)
		'four':  json.Any(4)
	}
	// 	numbers := {
	// 		'one':   1
	// 		'two':   2
	// 		'three': 3
	// 		'four':  4
	// 	}
	assert json.encode(numbers) == expected
	assert numbers.str() == expected
}

type ID = string
type GG = int

struct Message {
	id ID
	ij GG
}

fn test_encode_alias_struct() {
	expected := '{"id":"118499178790780929","ij":999998888}'
	msg := Message{'118499178790780929', 999998888}
	out := json.encode[Message](msg)
	assert out == expected
}

struct StByteArray {
	ba []u8
}

fn test_byte_array() {
	assert json.encode(StByteArray{ ba: [u8(1), 2, 3, 4, 5] }) == '{"ba":[1,2,3,4,5]}'
}

struct Bar {
	x string
}

fn bar[T](payload string) !Bar { // ?T doesn't work currently
	result := json.decode[T](payload)!
	return result
}

fn test_generic() {
	result := bar[Bar]('{"x":"test"}') or { Bar{} }
	assert result.x == 'test'
}

struct Foo[T] {
pub:
	name string
	data T
}

fn test_generic_struct() {
	foo_int := Foo[int]{'bar', 12}
	foo_enc := json.encode(foo_int)
	assert foo_enc == '{"name":"bar","data":12}'
	foo_dec := json.decode[Foo[int]](foo_enc)!
	assert foo_dec.name == 'bar'
	assert foo_dec.data == 12
}

type StringAlias = string

// TODO: encode_pretty array, sum types, struct, alias of struct and others...
struct Foo2 {
	ux8  u8
	ux16 u16
	ux32 u32
	ux64 u64
	sx8  i8
	sx16 i16
	sx32 int
	sx64 i64
	a    bool
	b    string
	c    StringAlias
}

fn test_pretty() {
	foo := Foo2{1, 2, 3, 4, -1, -2, -3, -4, true, 'abc', 'aliens'}
	assert json.encode_pretty(foo) == '{
  "ux8": 1,
  "ux16": 2,
  "ux32": 3,
  "ux64": 4,
  "sx8": -1,
  "sx16": -2,
  "sx32": -3,
  "sx64": -4,
  "a": true,
  "b": "abc",
  "c": "aliens"
}'
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
	assert s == '{"sub":{"a":1}}'
}

struct APrice {}

pub struct Association {
	association &Association = unsafe { nil }
	price       APrice
}

fn test_encoding_struct_with_pointers() {
	value := Association{
		association: &Association{
			price: APrice{}
		}
		price:       APrice{}
	}
	// println(value)
	assert json.encode(value) == '{"association":{"price":{}},"price":{}}'
}

pub struct City {
mut:
	name string
}

pub struct Country {
mut:
	cities []City
	name   string
}

struct Data {
mut:
	countries []Country
	users     map[string]User
	extra     map[string]map[string]int
}

fn test_nested_type() {
	data_expected := '{"countries":[{"cities":[{"name":"London"},{"name":"Manchester"}],"name":"UK"},{"cities":[{"name":"Donlon"},{"name":"Termanches"}],"name":"KU"}],"users":{"Foo":{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0,"pet_animals":"little foo"},"Boo":{"age":20,"nums":[5,3,1],"lastName":"Smith","IsRegistered":false,"type":4,"pet_animals":"little boo"}},"extra":{"2":{"n1":2,"n2":4,"n3":8,"n4":16},"3":{"n1":3,"n2":9,"n3":27,"n4":81}}}'
	data := Data{
		countries: [
			Country{
				name:   'UK'
				cities: [City{'London'}, City{'Manchester'}]
			},
			Country{
				name:   'KU'
				cities: [City{'Donlon'}, City{'Termanches'}]
			},
		]
		users:     {
			'Foo': User{
				age:           10
				nums:          [1, 2, 3]
				last_name:     'Johnson'
				is_registered: true
				typ:           0
				pets:          'little foo'
			}
			'Boo': User{
				age:           20
				nums:          [5, 3, 1]
				last_name:     'Smith'
				is_registered: false
				typ:           4
				pets:          'little boo'
			}
		}
		extra:     {
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
	assert out == data_expected
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
		title:  'Super Mega Game'
		player: Human{'Monke'}
		other:  [
			Entity(Item{'Pen'}),
			Item{'Cookie'},
			Animal.cat,
			'Stool',
			t,
		]
	}

	enc := json.encode(game)

	assert enc == '{"title":"Super Mega Game","player":{"name":"Monke"},"other":[{"tag":"Pen"},{"tag":"Cookie"},1,"Stool","${t.format_rfc3339()}"]}'
}

struct Foo3 {
	name string
	age  int @[omitempty]
}

// fn test_omit_empty() {
// 	foo := Foo3{'Bob', 0}
// 	assert json.encode(foo) == '{"name":"Bob"}'
// 	assert json.encode_pretty(foo) == '{
//   "name": "Bob"
// }'
// }

struct Foo31 {
	name string
	age  ?int
}

fn test_option_instead_of_omit_empty() {
	foo := Foo31{
		name: 'Bob'
	}
	assert json.encode_pretty(foo) == '{
  "name": "Bob"
}'
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

// fn test_encode_sumtype_defined_ahead() {
// 	ret := create_game_packet(&GamePacketData(GPScale{}))
// 	assert ret == '{"value":0}'
// }
