interface ParenShape {
	f() int
}

struct ParenSquare {}

fn (s ParenSquare) f() int {
	return 7
}

// Parentheses are the regression: keep them when formatting.
// vfmt off
fn paren_shape_option(value ParenSquare) ?ParenShape {
	return (value)
}

fn paren_shape_result(value &ParenSquare) !ParenShape {
	return ((value))
}

// vfmt on

struct ParenShapeHolder {
mut:
	shape ?ParenShape
}

// vfmt off
fn test_parenthesized_optional_interface_conversion() {
	value := ParenSquare{}
	pointer := &ParenSquare{}
	mut shape := ?ParenShape(none)
	shape = (value)
	assert shape?.f() == 7
	shape = ((pointer))
	assert shape?.f() == 7
	mut holder := ParenShapeHolder{}
	holder.shape = (ParenSquare{})
	assert holder.shape?.f() == 7
	option := paren_shape_option(value)?
	assert option.f() == 7
	result := paren_shape_result(pointer)!
	assert result.f() == 7
	cast_value := ?ParenShape((value))
	assert cast_value?.f() == 7
	cast_pointer := ?ParenShape(((pointer)))
	assert cast_pointer?.f() == 7
}
// vfmt on
