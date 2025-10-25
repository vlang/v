import db.sqlite

struct Test {
	value  f32
	value2 f64
}

fn test_main() {
	conn := sqlite.connect(':memory:')!

	data := Test{32.32, 64.64}

	sql conn {
		create table Test
		insert data into Test
	}!

	s := sql conn {
		select from Test
	}!

	assert s[0].value == 32.32
	assert s[0].value2 == 64.64
}
