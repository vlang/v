struct StructWithCReservedWord {
	error  string
	while  int
	extern int
	switch bool
}

fn test_structs_that_have_fields_that_are_reserved_c_words_can_be_iterated() {
	foo := StructWithCReservedWord{
		error: 'this is an error message'
		while: 123
		extern: 456
		switch: true
	}
	$for field in StructWithCReservedWord.fields {
		$if field.typ is string {
			println(foo.$(field.name))
		}
	}
	assert foo.switch
	assert foo.extern == 456
	assert foo.while == 123
	assert foo.error == 'this is an error message'
	println(foo)
}
