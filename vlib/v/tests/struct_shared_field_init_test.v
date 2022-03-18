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

struct DD {
	e shared []int
}

fn get_dd() DD {
	return DD{
		e: [1, 2, 3]
	}
}

fn test_other() {
	d := get_dd()
	assert d.e[0] == 1 // TODO this should be an error
	e := lock d.e {
		d.e
	}
	assert e == [1, 2, 3]
}
