type CEnum = int

enum Enum {
	value = 1
}

fn foo(n int) string {
	return '${n}'
}

fn bar(n CEnum) string {
	return '${n}'
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

struct Foo {
	x int
	y string
}

type Alias = Foo

fn test_cast_to_alias_of_ref_struct() {
	foo := &Foo(unsafe { nil })
	println(typeof(foo).name)
	assert typeof(foo).name == '&Foo'

	bar := &Alias(unsafe { nil })
	println(typeof(bar).name)
	assert typeof(bar).name == '&Alias'
}

type Bar1 = Foo
type Bar2 = Foo
type Bar3 = Foo

fn test_cast_to_from_alias() {
	bar2 := Bar2{
		x: 100
	}
	foo := Foo(bar2)
	bar1 := Bar1(bar2)
	bar3 := Bar3(bar2)
	assert typeof(foo).name == 'Foo'
	assert typeof(bar1).name == 'Bar1'
	assert typeof(bar2).name == 'Bar2'
	assert typeof(bar3).name == 'Bar3'

	foo1_1 := Foo(foo)
	bar1_1 := Bar1(foo)
	bar2_1 := Bar2(foo)
	bar3_1 := Bar3(foo)
	assert typeof(foo1_1).name == 'Foo'
	assert typeof(bar1_1).name == 'Bar1'
	assert typeof(bar2_1).name == 'Bar2'
	assert typeof(bar3_1).name == 'Bar3'
}
