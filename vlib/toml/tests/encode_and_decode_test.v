import toml

enum Rank {
	low
	medium
	high
}

struct Planet {
	name       string
	population u64
	size       f64
	avg_temp   int
	has_water  bool
	rank       Rank
}

struct Employee {
mut:
	name     string
	age      int
	salary   f32
	is_human bool
	rank     Rank
}

fn test_encode_and_decode() {
	p := Planet{'Mars', 0, 144.8, -81, true, .high}
	s := 'name = "Mars"
population = 0
size = 144.8
avg_temp = -81
has_water = true
rank = 2'

	assert toml.encode[Planet](p) == s
	assert toml.decode[Planet](s)! == p
}

pub fn (e Employee) to_toml() string {
	mut mp := map[string]toml.Any{}
	mp['name'] = toml.Any(e.name)
	mp['age'] = toml.Any(e.age)
	mp['salary'] = toml.Any(f32(e.salary) + 5000.0)
	mp['is_human'] = toml.Any(e.is_human)
	mp['rank'] = toml.Any(int(e.rank) + 1)
	return mp.to_toml()
}

pub fn (mut e Employee) from_toml(any toml.Any) {
	mp := any.as_map()
	e.name = mp['name'] or { toml.Any('') }.string()
	e.age = mp['age'] or { toml.Any(0) }.int()
	e.salary = mp['salary'] or { toml.Any(0) }.f32() - 15000.0
	e.is_human = mp['is_human'] or { toml.Any(false) }.bool()
	e.rank = unsafe { Rank(mp['rank'] or { toml.Any(0) }.int() - 2) }
}

fn test_custom_encode_and_decode() {
	x := Employee{'Peter', 28, 95000.5, true, .medium}
	s := toml.encode[Employee](x)
	eprintln('Employee x: ${s}')
	assert s == r'name = "Peter"
age = 28
salary = 100000.5
is_human = true
rank = 2'

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
	assert y.rank == .low
}
