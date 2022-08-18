module main

struct Author {
	username string
	age      int
}

fn cool_author() Author {
	return Author{
		username: 'Terisback'
		age: 18
	}
}

fn test_struct_init_with_update_expr() {
	mut o := Author{
		...cool_author()
		age: 21
	}
	println(o)
	assert o.username == 'Terisback'
	assert o.age == 21
}
