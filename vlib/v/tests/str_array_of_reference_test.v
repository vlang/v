fn test_str_array_of_reference() {
	names := ['John', 'Paul', 'George', 'Ringo']
	a := unsafe { [&names[0], &names[1]] }
	println(a[0])
	println(a)
	assert '$a' == "[&'John', &'Paul']"
}
