struct Foo {
	immutable int
mut:
	test string
	name string
}

fn comptime_field_selector_read<T>() []string {
	mut t := T{}
	t.name = '2'
	t.test = '1'
	mut value_list := []string{}
	$for f in T.fields {
		$if f.typ is string {
			value_list << t.$(f.name)
		}
	}
	return value_list
}

fn test_comptime_field_selector_read() {
	assert comptime_field_selector_read<Foo>() == ['1', '2']
}

fn comptime_field_selector_write<T>() T {
	mut t := T{}
	$for f in T.fields {
		$if f.typ is string {
			t.$(f.name) = '1'
		}
		$if f.typ is int {
			t.$(f.name) = 1
		}
	}
	return t
}

fn test_comptime_field_selector_write() {
	res := comptime_field_selector_write<Foo>()
	assert res.immutable == 1
	assert res.test == '1'
	assert res.name == '1'
}

struct Foo2 {
	f Foo
}

fn nested_with_parentheses<T>() T {
	mut t := T{}
	$for f in T.fields {
		$if f.typ is Foo {
			t.$(f.name).test = '1'
		}
	}
	return t
}

fn test_nested_with_parentheses() {
	res := nested_with_parentheses<Foo2>()
	assert res.f.test == '1'
}
