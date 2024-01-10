import alias_to_another_module

struct MyStruct {
	myfield alias_to_another_module.MyAlias
}

fn test_using_struct_with_alias() {
	m := MyStruct{
		myfield: alias_to_another_module.MyAlias{1, 2, 3}
	}
	dump(m)
	assert m.myfield.alias_method() == 42
	assert m.myfield.some_method() == 1005
}
