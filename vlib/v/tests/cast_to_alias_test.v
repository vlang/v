module main

type CEnum = int

enum Enum {
	value = 1
}

fn foo(n int) string {
	return '$n'
}

fn bar(n CEnum) string {
	return '$n'
}

fn test_cast_to_alias() {
	e := Enum.value
	mut ret_str := ''

	ret_str = foo(int(e))
	println(ret_str)
	assert ret_str == '1'

	ret_str = bar(int(e))
	println(ret_str)
	assert ret_str == '1'

	ret_str = foo(CEnum(e))
	println(ret_str)
	assert ret_str == '1'

	ret_str = bar(CEnum(e))
	println(ret_str)
	assert ret_str == '1'
}
