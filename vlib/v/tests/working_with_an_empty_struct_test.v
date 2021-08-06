struct EmptyStruct {}

pub fn (f EmptyStruct) str() string {
	return 'EmptyStruct{}'
}

fn new_s() EmptyStruct {
	println('>get_foo')
	return EmptyStruct{}
}

fn test_using_an_empty_struct_compiles_and_works() {
	s := new_s()
	eprintln('s: ${s}')
	assert true
}
