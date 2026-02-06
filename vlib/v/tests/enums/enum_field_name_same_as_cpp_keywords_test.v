enum MyEnum {
	catch
	class
	dynamic_cast
	static_cast
	operator
	virtual
}

fn test_cpp_keywords_used_as_enum_values() {
	e := MyEnum.class
	assert e.str() == 'class'
}
