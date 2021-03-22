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
