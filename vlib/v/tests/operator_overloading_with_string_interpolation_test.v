struct Vec {
	x int
	y int
}

pub fn (a Vec) str() string {
	return '{$a.x, $a.y}'
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn (a Vec) - (b Vec) Vec {
	return Vec{a.x - b.x, a.y - b.y}
}

fn (a Vec) * (b Vec) Vec {
	return Vec{a.x * b.x, a.y * b.y}
}

fn (a Vec) / (b Vec) Vec {
	return Vec{a.x / b.x, a.y / b.y}
}

fn (a Vec) % (b Vec) Vec {
	return Vec{a.x % b.x, a.y % b.y}
}

fn (a Vec) > (b Vec) bool {
	return a.x > b.x && a.y > b.y
}

fn (a Vec) < (b Vec) bool {
	return a.x < b.x && a.y < b.y
}

fn test_operator_overloading_with_string_interpolation() {
	a := Vec{2, 3}
	b := Vec{4, 5}
	c := a + b
	assert a.x + b.x == c.x
	assert a.y + b.y == c.y
	////// /////
	d := a - b
	assert a.x - b.x == d.x
	assert a.y - b.y == d.y
	////// /////
	e := a * b
	assert a.x * b.x == e.x
	assert a.y * b.y == e.y
	////// /////
	f := a / b
	assert a.x / b.x == f.x
	assert a.y / b.y == f.y
	////// /////
	g := a % b
	assert a.x % b.x == g.x
	assert a.y % b.y == g.y
	////// /////
	assert b > a == true
	assert a < b == true
	////// /////
	assert c.str() == '{6, 8}'
	assert d.str() == '{-2, -2}'
	assert e.str() == '{8, 15}'
	assert f.str() == '{0, 0}'
	assert g.str() == '{2, 3}'
}
