module testml

import (
	yaml
	json
)

struct TestML {
	var []TestML
}

struct TestMLVar {
	name		string
	var		 	string
	var_type 	TestMLVarType
}

fn (t TestMLVar) type TestMLType {
	return t.var_type
}

fn (t TestMLVar) var string{
	return t.var
}

enum TestMLVarType {
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