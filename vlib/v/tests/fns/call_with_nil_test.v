module main

interface ISomeStruct {
	something string
}

struct SomeStruct {
	something string
}

fn test(s &ISomeStruct) {
	assert '${s}' == '&nil'
	if s == unsafe { nil } {
		assert true
	} else {
		assert false
	}
}

fn test_main() {
	y := unsafe { nil }
	test(y)
	test(unsafe { nil })
}
