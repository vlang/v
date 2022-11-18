import toml

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
pub mut:
	name     string
	age      int
	salary   f32
	is_human bool
	title    JobTitle
}

fn (e Employee) to_toml() string {
	mut mp := map[string]toml.Any{}
	mp['name'] = toml.Any(e.name)
	mp['age'] = toml.Any(e.age)
	mp['salary'] = toml.Any(e.salary)
	mp['is_human'] = toml.Any(e.is_human)
	mp['title'] = toml.Any(int(e.title))
	return mp.to_toml()
}

fn (mut e Employee) from_toml(any toml.Any) {
	mp := any.as_map()
	e.name = mp['name'] or { toml.Any('') }.string()
	e.age = mp['age'] or { toml.Any(0) }.int()
	e.salary = mp['salary'] or { toml.Any(0) }.f32()
	e.is_human = mp['is_human'] or { toml.Any(false) }.bool()
	e.title = unsafe { JobTitle(mp['title'] or { toml.Any(0) }.int()) }
}

fn test_encode_and_decode() {
	x := Employee{'Peter', 28, 95000.5, true, .worker}
	s := toml.encode<Employee>(x)
	eprintln('Employee x: ${s}')
	assert s == r'name = "Peter"
age = 28
salary = 95000.5
is_human = true
title = 2'

	y := toml.decode<Employee>(s) or {
		println(err)
		assert false
		return
	}
	eprintln('Employee y: ${y}')
	assert y.name == 'Peter'
	assert y.age == 28
	assert y.salary == 95000.5
	assert y.is_human == true
	assert y.title == .worker
}
