import json

struct Test {
	optional_alias  ?MyAlias  // primitive
	optional_struct ?MyAlias2 // complex
}

struct Complex {
	a int = 3
}

type MyAlias = int
type MyAlias2 = Complex

fn test_empty() {
	test := Test{}
	encoded := json.encode(test)
	assert dump(encoded) == '{}'
}

fn test_value() {
	test := Test{
		optional_alias: 1
	}
	encoded := json.encode(test)
	assert dump(encoded) == '{"optional_alias":1}'
}

fn test_value_2() {
	test := Test{
		optional_alias: 1
		optional_struct: Complex{
			a: 1
		}
	}
	encoded := json.encode(test)
	assert dump(encoded) == '{"optional_alias":1,"optional_struct":{"a":1}}'
}
