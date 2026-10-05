struct InlineAccess {
	a         int
	mut     b int
	pub     c int
	pub mut d int
	e         int = 5
}

fn test_inline_field_modifiers_preserve_layout_and_mutability() {
	mut value := InlineAccess{1, 2, 3, 4, 5}
	value.b++
	value.d++
	assert value.a == 1
	assert value.b == 3
	assert value.c == 3
	assert value.d == 5
	assert value.e == 5
	assert __offsetof(InlineAccess, a) < __offsetof(InlineAccess, b)
	assert __offsetof(InlineAccess, b) < __offsetof(InlineAccess, c)
	assert __offsetof(InlineAccess, c) < __offsetof(InlineAccess, d)
	assert __offsetof(InlineAccess, d) < __offsetof(InlineAccess, e)
	mut names := []string{}
	mut mutable := []string{}
	mut public := []string{}
	$for field in InlineAccess.fields {
		names << field.name
		$if field.is_mut {
			mutable << field.name
		}
		$if field.is_pub {
			public << field.name
		}
	}
	assert names == ['a', 'b', 'c', 'd', 'e']
	assert mutable == ['b', 'd']
	assert public == ['c', 'd']
}
