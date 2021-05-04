struct MyStructInt {
	x int
}

type ZZInt = int
type ZZMyStructInt = MyStructInt

type PZZInt = &int
type PZZMyStructInt = &MyStructInt

type PPZZInt = &&int
type PPZZMyStructInt = &&MyStructInt

type PPPZZInt = &&&int
type PPPZZMyStructInt = &&&MyStructInt

fn test_alias_of_pointer_types() {
	size_of_int := int(sizeof(int))
	size_of_int_ptr := int(sizeof(&int))
	dump(size_of_int)
	dump(size_of_int_ptr)
	eprintln('--------------------------')
	dump(sizeof(ZZInt))
	dump(sizeof(ZZMyStructInt))
	dump(sizeof(PZZInt))
	dump(sizeof(PZZMyStructInt))
	dump(sizeof(PPZZInt))
	dump(sizeof(PPZZMyStructInt))
	dump(sizeof(PPPZZInt))
	dump(sizeof(PPPZZMyStructInt))
	//
	assert sizeof(ZZInt) == sizeof(int)
	assert sizeof(ZZMyStructInt) == sizeof(int)
	//
	assert sizeof(PZZInt) == sizeof(voidptr)
	assert sizeof(PZZMyStructInt) == sizeof(voidptr)
	assert sizeof(PPZZInt) == sizeof(voidptr)
	assert sizeof(PPZZMyStructInt) == sizeof(voidptr)
}

fn test_calling_a_function_expecting_a_mut_alias() {
	eprintln('------------------------')
	mut s := &MyStructInt{456}
	mut ps := PZZMyStructInt(s)
	dump(voidptr(s))
	dump(voidptr(ps))
	eprintln('------------------------')
	dump(&MyStructInt(ps))
	res := mut_alias(mut ps)
	dump(&MyStructInt(ps))
	// the alias `ps` is now changed and points to another object
	assert res == 123
	assert s.x == 456 // should remain the same
	assert (&MyStructInt(ps)).x == 789
	assert u64(voidptr(s)) != u64(voidptr(ps))
	dump(voidptr(s))
	dump(voidptr(ps))
	eprintln('------------------------')
}

// do not delete this, its generated code eases comparisons with mut_alias
fn mut_struct(mut p ZZMyStructInt) int {
	dump(ptr_str(voidptr(p)))
	return 999
}

fn mut_alias(mut ps PZZMyStructInt) int {
	//	dump(ptr_str(voidptr(ps)))
	another := &MyStructInt{789}
	//	dump(ptr_str(voidptr(another)))
	ps = PZZMyStructInt(another)
	//	dump(ptr_str(voidptr(ps)))
	return 123
}
