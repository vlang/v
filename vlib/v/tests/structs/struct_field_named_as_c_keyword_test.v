struct Foo {
	do fn () = unsafe { nil }
}

struct Foo2 {
	do fn () = unsafe { nil }
}

type CallAlias = Foo | Foo2

fn get() CallAlias {
	return Foo{
		do: fn () {
			println('cool')
		}
	}
}

fn test_struct_with_a_field_named_with_a_c_keyword() {
	a := get().do
	a()
}
