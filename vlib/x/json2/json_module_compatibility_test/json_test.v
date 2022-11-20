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
	sub_employee SubEmployee
}

struct SubEmployee {
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
	s := json.encode<Employee>(x)
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2,"sub_employee":{"name":"João","age":0,"salary":0.0,"title":0}}'
	// y := json.decode<Employee>(s) or {
	// 	println(err)
	// 	assert false
	// 	return
	// }
	// assert y.name == 'Peter'
	// assert y.age == 28
	// assert y.salary == 95000.5
	// assert y.title == .worker
	// x := Employee{'Peter', 28, 95000.5, .worker}
	// s := json.encode<Employee>(x)
	// assert s == '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	// // y := json.decode<Employee>(s) or {
	// // 	println(err)
	// // 	assert false
	// // 	return
	// // }
	// // assert y.name == 'Peter'
	// // assert y.age == 28
	// // assert y.salary == 95000.5
	// // assert y.title == .worker
}

// const currency_id = 'cconst'

// struct Price {
// 	net         f64
// 	currency_id string [json: currencyId] = currency_id
// }

// struct User2 {
// mut:
// 	age      int
// 	nums     []int
// 	reg_date time.Time
// }

// // User struct needs to be `pub mut` for now in order to access and manipulate values
struct User {
pub mut:
	age           int
	nums          []int
	last_name     string [json: lastName]
	is_registered bool   [json: IsRegistered]
	typ           int    [json: 'type']
	pets          string [json: 'pet_animals'; raw]
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
	out := json.encode<User>(usr)
	// println(out)
	assert out == expected
	// Test json.encode on mutable pointers
	assert usr.foo() == expected
}

// struct Color {
// pub mut:
// 	space string
// 	point string [raw]
// }

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
	// 	out := json.encode(numbers)
	out := numbers.str()
	assert out == expected
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
	out := json.encode<Message>(msg)
	assert out == expected
}

struct StByteArray {
	ba []byte
}

fn test_byte_array() {
	assert json.encode(StByteArray{ ba: [byte(1), 2, 3, 4, 5] }) == '{"ba":[1,2,3,4,5]}'
}
