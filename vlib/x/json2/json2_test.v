import x.json2 as json
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
	mut mp := map[string]json.Any{}
	mp['name'] = json.Any(e.name)
	mp['age'] = json.Any(e.age)
	mp['salary'] = json.Any(e.salary)
	mp['title'] = json.Any(int(e.title))
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

fn (mut e Employee) from_json(any json.Any) {
	mp := any.as_map()
	e.name = mp['name'] or { json.Any('') }.str()
	e.age = mp['age'] or { json.Any(0) }.int()
	e.salary = mp['salary'] or { json.Any(0) }.f32()
	e.title = unsafe { JobTitle(mp['title'] or { json.Any(0) }.int()) }
}

// ! BUGFIX
// fn test_simplegg() {
// 	// x := EmployeeOp{'Peter', 28, 95000.5, .worker}
// 	x := EmployeeOp{
// 		name: 'vshfvhsd'
// 	}
// 	s := json.encode<EmployeeOp>(x)
// 	assert s == '{"name":"vshfvhsd","age":0,"salary":0.0,"title":0.0}'
// 	// y := json.decode<EmployeeOp>(s) or {
// 	// 	println(err)
// 	// 	assert false
// 	// 	return
// 	// }
// 	// assert y.name == 'Peter'
// 	// assert y.age == 28
// 	// assert y.salary == 95000.5
// 	// assert y.title == .worker
// }

fn test_fast_raw_decode() {
	s := '{"name":"Peter","age":28,"salary":95000.5,"title":2}'
	o := json.fast_raw_decode(s) or {
		assert false
		json.Any(json.null)
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
	mut obj := json.raw_decode(message) or {
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

fn (mut u User2) from_json(an json.Any) {
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

struct User2 {
pub mut:
	age      int
	nums     []int
	reg_date time.Time
}

struct User {
pub mut:
	age           int
	nums          []int
	last_name     string [json: lastName]
	is_registered bool   [json: IsRegistered]
	typ           int    [json: 'type']
	pets          string [json: 'pet_animals'; raw]
}

fn (mut u User) from_json(an json.Any) {
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
		'age': json.Any(u.age)
	}
	mp['nums'] = u.nums.map(json.Any(it))
	mp['lastName'] = json.Any(u.last_name)
	mp['IsRegistered'] = json.Any(u.is_registered)
	mp['type'] = json.Any(u.typ)
	mp['pet_animals'] = json.Any(u.pets)
	return mp.str()
}

struct Color {
pub mut:
	space string
	point string [raw]
}

fn (mut c Color) from_json(an json.Any) {
	mp := an.as_map()
	$for field in Color.fields {
		match field.name {
			'space' { c.space = mp[field.name] or { 0 }.str() }
			'point' { c.point = mp[field.name] or { 0 }.str() }
			else {}
		}
	}
}
