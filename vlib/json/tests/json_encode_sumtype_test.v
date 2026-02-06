import json

struct Test {
	optional_sumtype ?MySumtype
}

type MySumtype = int | string

fn test_simple() {
	test := Test{}
	encoded := json.encode(test)
	assert dump(encoded) == '{}'
}
