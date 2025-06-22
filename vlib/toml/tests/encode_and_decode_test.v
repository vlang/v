import toml

enum JobTitle {
	worker
	executive
	manager
}

struct Pet {
	name           string
	nicknames      []string
	age            u64
	income         int
	height         f32
	has_furr       bool
	title          JobTitle
	address        Address
	meal_frequency map[string]int
	// *¹ Currently it is only possible to decode a single nested struct generically.
	// As soon as we decode another nested struct (e.g. within this struct, like `contact` below)
	// or only one nested struct within another struct, it results in wrong values or errors.
	// Related issue: https://github.com/vlang/v/issues/18110
	// contact Contact
}

struct Address {
	street string
	city   string
}

// *¹
/*
struct Contact {
	phone string
}*/

struct AnyStruct {
	val toml.Any
}

struct Employee {
mut:
	name     string
	age      int
	salary   f32
	is_human bool
	title    JobTitle
}

struct Arrs {
	strs  []string
	bools []bool
	ints  []int
	i64s  []i64
	u64s  []u64
	f32s  []f32
	f64s  []f64
	dts   []toml.DateTime
	dates []toml.Date
	times []toml.Time
}

// individual because toml.decode[Foo](str)! == foo is false
struct AnyArr {
	arr []toml.Any
}

fn test_encode_and_decode() {
	// *¹
	// p := Pet{'Mr. Scratchy McEvilPaws', ['Freddy', 'Fred', 'Charles'], 8, -1, 0.8, true, .manager, Address{'1428 Elm Street', 'Springwood'}, Contact{'123-456-7890'}}
	p := Pet{'Mr. Scratchy McEvilPaws', ['Freddy', 'Fred', 'Charles'], 8, -1, 0.8, true, .manager, Address{'1428 Elm Street', 'Springwood'}, {
		'bones':  2
		'kibble': 5
	}}
	s := 'name = "Mr. Scratchy McEvilPaws"
nicknames = [
  "Freddy",
  "Fred",
  "Charles"
]
age = 8
income = -1
height = 0.8
has_furr = true
title = 2
address = { street = "1428 Elm Street", city = "Springwood" }
meal_frequency = { bones = 2, kibble = 5 }'
	// contact = { phone = "123-456-7890" }' // *¹

	assert toml.encode[Pet](p) == s
	assert toml.decode[Pet](s)! == p
}

fn test_encode_and_decode_any() {
	a := AnyStruct{toml.Any(10)}
	s := 'val = 10'

	assert toml.encode[AnyStruct](a) == s
	assert toml.decode[AnyStruct](s)!.val.int() == 10
}

pub fn (e Employee) to_toml() string {
	mut mp := map[string]toml.Any{}
	mp['name'] = toml.Any(e.name)
	mp['age'] = toml.Any(e.age)
	mp['is_human'] = toml.Any(e.is_human)
	// Change some values to assert that the custom method is used instead of generic encoding.
	mp['salary'] = toml.Any(f32(e.salary) + 5000.0)
	mp['title'] = toml.Any(int(e.title) + 1)
	return mp.to_toml()
}

pub fn (mut e Employee) from_toml(any toml.Any) {
	mp := any.as_map()
	e.name = mp['name'] or { toml.Any('') }.string()
	e.age = mp['age'] or { toml.Any(0) }.int()
	e.is_human = mp['is_human'] or { toml.Any(false) }.bool()
	// Change some values to assert that the custom method is used instead of generic decoding.
	e.salary = mp['salary'] or { toml.Any(0) }.f32() - 15000.0
	e.title = unsafe { JobTitle(mp['title'] or { toml.Any(0) }.int() - 2) }
}

fn test_custom_encode_and_decode() {
	x := Employee{'Peter', 28, 95000.5, true, .executive}
	s := toml.encode[Employee](x)
	eprintln('Employee x: ${s}')
	assert s == r'name = "Peter"
age = 28
is_human = true
salary = 100000.5
title = 2'

	y := toml.decode[Employee](s) or {
		println(err)
		assert false
		return
	}
	eprintln('Employee y: ${y}')
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 85000.5
	assert y.is_human == true
	assert y.title == .worker
}

struct Example1 {
	arr []Problem
}

struct Example2 {
	arr []Problem
}

struct Problem {
	x int
}

pub fn (example Example1) to_toml() string {
	return '[This is Valid]'
}

pub fn (problem Problem) to_toml() string {
	return 'a problem'
}

fn test_custom_encode_of_complex_struct() {
	assert toml.encode(Example1{}) == '[This is Valid]'
	assert toml.encode(Example2{[Problem{}, Problem{}]}) == 'arr = [
  "a problem",
  "a problem"
]'
}

struct Example3 {
	arr_arr [][]Problem
}

struct Example4 {
	mp map[string]Problem
}

pub fn (example Example3) to_toml() string {
	return '[This is Valid]'
}

pub fn (example Example4) to_toml() string {
	return '[This is Valid]'
}

fn test_custom_encode_of_nested_complex_struct() {
	assert toml.encode(Example3{}) == '[This is Valid]'
	assert toml.encode(Example4{}) == '[This is Valid]'
}

struct Example5 {
	mp map[string]Problem
}

fn test_map_encode_of_complex_struct() {
	mut mp := map[string]Problem{}
	mp['key_one'] = Problem{}
	mp['key_two'] = Problem{}
	assert toml.encode(Example5{ mp: mp }) == 'mp = { key_one = "a problem", key_two = "a problem" }'
}

struct Example6 {
	ptr voidptr
	r   rune
}

fn test_encode_for_exotic_types() {
	assert toml.encode(Example6{ ptr: unsafe { &voidptr(nil) }, r: `🚀` }) == 'ptr = "0x0"\nr = "🚀"'
}

fn test_array_encode_decode() {
	a := Arrs{
		strs:  ['foo', 'bar']
		bools: [true, false]
		ints:  [-1, 2]
		i64s:  [i64(-2)]
		u64s:  [u64(123)]
		f32s:  [f32(1.0), f32(2.5)]
		f64s:  [100000.5, -123.0]
		dts:   [toml.DateTime{'1979-05-27T07:32:00Z'}, toml.DateTime{'1979-05-27T07:32:00Z'}]
		dates: [toml.Date{'1979-05-27'}, toml.Date{'2022-12-31'}]
		times: [toml.Time{'07:32:59'}, toml.Time{'17:32:04'}]
	}

	s := 'strs = [
  "foo",
  "bar"
]
bools = [
  true,
  false
]
ints = [
  -1,
  2
]
i64s = [
  -2
]
u64s = [
  123
]
f32s = [
  1.0,
  2.5
]
f64s = [
  100000.5,
  -123.0
]
dts = [
  1979-05-27T07:32:00Z,
  1979-05-27T07:32:00Z
]
dates = [
  1979-05-27,
  2022-12-31
]
times = [
  07:32:59,
  17:32:04
]'

	assert toml.encode[Arrs](a) == s
	assert toml.decode[Arrs](s)! == a

	any_a := AnyArr{[toml.Any(10), 20, 30]}
	any_s := 'arr = [
  10,
  20,
  30
]'

	assert toml.encode[AnyArr](any_a) == any_s
	assert toml.decode[AnyArr](any_s)!.arr.map(it.int()) == [10, 20, 30]
}

fn test_decode_doc() {
	doc := toml.parse_text('name = "Peter"
age = 28
is_human = true
salary = 100000.5
title = 2')!
	e := doc.decode[Employee]()!
	assert e.name == 'Peter'
	assert e.age == 28
	assert e.salary == 100000.5
	assert e.is_human == true
	assert e.title == .manager
}

fn test_unsupported_type() {
	s := 'name = "Peter"'
	err_msg := 'toml.decode: expected struct, found '
	if _ := toml.decode[string](s) {
		assert false
	} else {
		assert err.msg() == err_msg + 'string'
	}
	if _ := toml.decode[[]string](s) {
		assert false
	} else {
		assert err.msg() == err_msg + '[]string'
	}
	if _ := toml.decode[int](s) {
		assert false
	} else {
		assert err.msg() == err_msg + 'int'
	}
	if _ := toml.decode[[]f32](s) {
		assert false
	} else {
		assert err.msg() == err_msg + '[]f32'
	}
	// ...

	doc := toml.parse_text('name = "Peter"')!
	assert doc.value('name').string() == 'Peter'
	if _ := doc.decode[string]() {
		assert false
	} else {
		assert err.msg() == 'Doc.decode: expected struct, found string'
	}
}
