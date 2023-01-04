struct Animal {
	class_name string = 'Animal'
pub mut:
	interf Adoptable
}

interface Adoptable {
	class_name string
	age int
	test int
}

struct Cat {
pub mut:
	class_name string = 'Cat'
	age        int    = 2
	test       int    = 2
}

fn new_adoptable() Adoptable {
	return Cat{}
}

fn test_interface_fn_return_with_struct_init() {
	mut a := Animal{
		interf: new_adoptable()
	}
	println(a.interf.class_name)
	assert a.interf.class_name == 'Cat'
	println(a.interf.age)
	assert a.interf.age == 2
	println(a.interf.test)
	assert a.interf.test == 2
}
