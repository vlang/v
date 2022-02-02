struct AA {
	b shared BB
}

struct BB {
	a &int
}

struct CC {
	a BB
}

fn test_struct_shared_field_init() {
	a := 3
	table := &AA{
		b: BB{&a}
	}
	c := CC{
		a: table.b
	}
	assert *c.a.a == 3
}
