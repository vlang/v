fn test_main() {
	pi := 3.14
	type_ := typeof(pi).name
	assert typeof(type_).name == 'string'
	assert typeof(typeof(pi).name).name == 'string'
}
