struct Foo {
	foo string
	bar ?string
}

fn (f Foo) str() string {
	mut ret := f.foo
	bar_value := f.bar as string
	if bar_value == '' {
		return ret
	}
	return ret + '%' + bar_value
}

fn test_selector_expr_as_cast() {
	ff := Foo{
		foo: 'Fooooo'
	}
	assert '${ff}' == 'Fooooo'
}

type Type27403 = TypeBasic27403 | TypeFunc27403

struct TypeBasic27403 {
	name string
}

struct TypeFunc27403 {
	variadic_type ?Type27403
}

fn option_sumtype_selector_as_sumtype(tf TypeFunc27403) Type27403 {
	return tf.variadic_type as Type27403
}

fn option_sumtype_selector_as_variant(tf TypeFunc27403) TypeBasic27403 {
	return tf.variadic_type as TypeBasic27403
}

fn test_option_sumtype_selector_expr_as_cast() {
	tf := TypeFunc27403{
		variadic_type: Type27403(TypeBasic27403{
			name: 'v'
		})
	}
	assert (option_sumtype_selector_as_sumtype(tf) as TypeBasic27403).name == 'v'
	assert option_sumtype_selector_as_variant(tf).name == 'v'
}
