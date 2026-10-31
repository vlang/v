// vtest vflags: -w
import json2

struct Test {
	optional_sumtype ?MySumtype
}

type MySumtype = int | string

fn test_simple() {
	test := Test{}
	encoded := json2.encode(test, escape_unicode: true)
	assert dump(encoded) == '{}'
}
