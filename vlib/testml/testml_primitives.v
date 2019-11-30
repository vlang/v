module testml

struct TestML {
	
}

struct TestMLVar {
	var_type TestMLType
}

enum TestMLType {
	str
	num
	regex
	null
	list
	type_bool
	hash
	error
	func
	type_none
	native
}