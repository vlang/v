module main

struct Animal {
	Duck
	id int
}

struct Duck {
	RedDuck
	action string
}

struct RedDuck {
	color string
}

fn test_struct_init_with_multi_nested_embed_update() {
	d := Animal{Duck{RedDuck{'red'}, 'fly'}, 1}
	println(d)

	e := Animal{
		...d
		id: 2
	}
	println(e)
	assert d.id == 1
	assert e.id == 2
}
