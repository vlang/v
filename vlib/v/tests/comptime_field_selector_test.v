struct Foo {
mut:
	test string
	name string
}

fn comptime_field_selector<T>() []string {
	mut t := T{}
	t.name = '2'
	t.test = '1'
	mut value_list := []string{}
	$for f in T.fields {
		$if f.typ is string {
			value_list << t.$f.name
		}
	}
	return value_list
}

fn test_comptime_field_selector() {
	assert comptime_field_selector<Foo>() == ['1', '2']
}
